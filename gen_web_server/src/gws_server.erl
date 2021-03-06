%% @author LMA
%% @doc @todo Add description to gws_server.


-module(gws_server).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3]).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

%% lock - holds the listening socket
%% socket - holds the dedicated socket
%% request_line, headers, body, content_remaining - for HTTP protocol
%% callback - the name of the behaviour implementation module
%% user_data - application specific data to be passed to callback module
%% parent - holds the PID of gws_connection_sup process
-record(state, {lsock, socket, request_line, headers = [],
				body = <<>>, content_remaining = 0,
				callback, user_data, parent}).

%% ====================================================================
%% API functions
start_link(Callback, LSock, UserArgs) ->
	gen_server:start_link(?MODULE,
						  [Callback, LSock, UserArgs, self()], []). % self is typically the gws_connection_sup process

%% ====================================================================
%% gen_server callbacks
init([Callback, LSock, UserArgs, Parent]) ->
	{ok, UserData} = Callback:init(UserArgs),
	State = #state{lsock = LSock, callback = Callback,
				   user_data = UserData, parent = Parent},
	{ok, State, 0}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({http, _Sock, {http_request, _, _, _} = Request}, State) ->
	% handles request lines
	inet:setopts(State#state.socket, [{active, once}]), % needed to continue reading from socket
	{noreply, State#state{request_line = Request}};
handle_info({http, _Sock, {http_request, _, Name, _, Value}}, State) ->
	% handles headers
	inet:setopts(State#state.socket, [{active, once}]), % needed to continue reading from socket
	{noreply, header(Name, Value, State)}; % accumulates the headers in a list in server state
handle_info({http, _Sock, http_eoh}, #state{content_remaining = 0} = State) ->
	% handles end of header, body is empty
	{stop, normal, handle_http_request(State)};
handle_info({http, _Sock, http_eoh}, State) ->
	% handles end of header, prepares for body
	% switching to {packet, raw} to stop parsing data as HTTP format, 
	% data would end up in below handle_info as {tcp, Socket, Data}
	inet:setopts(State#state.socket, [{active, once}, {packet, raw}]),
	{noreply, State};
handle_info({tcp, _Sock, Data}, State) when is_binary(Data) ->
	ContentRem = State#state.content_remaining - byte_size(Data),
	Body = list_to_binary([State#state.body, Data]),
	NewState = State#state{body = Body,
						   content_remaining = ContentRem},
	if	(ContentRem > 0) ->
			inet:setopts(State#state.socket, [{active, once}]), % needed to continue reading from socket
			{noreply, NewState};
		true ->
			{stop, normal, handle_http_request(NewState)}
	end;
handle_info({tcp_closed, _Sock}, State) ->
	{stop, normal, State};
handle_info(timeout, #state{lsock = LSock, parent = Parent} = State) ->
	% waits for connection and starts new handler
	% typically when init/1 completes
	{ok, Socket} = gen_tcp:accept(LSock),
	gws_connection_sup:start_child(Parent),
	inet:setopts(Socket, [{active, once}]), % needed to continue reading from socket
	{noreply, State#state{socket = Socket}}. % dedicated socket is set to {active,once}

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

	
%% ====================================================================
%% Internal functions
%% ====================================================================
header('Content-Length' = Name, Value, State) ->
	% remember content length for later
	ContentLength = list_to_integer(binary_to_list(Value)),
	State#state{content_remaining = ContentLength,
				headers = [{Name, Value} | State#state.headers]};
header(<<"Expect">> = Name, <<"100-continue">> = Value, State) ->
	% must send HTTP reply "100 Continue" so that client will not pause
	gen_tcp:send(State#state.socket, gen_web_server:http_reply(100)),
	State#state{headers = [{Name, Value} | State#state.headers]};
header(Name, Value, State) ->
	State#state{headers = [{Name, Value} | State#state.headers]}.

handle_http_request(#state{callback = Callback,
						   request_line = Request,
						   headers = Headers,
						   body = Body,
						   user_data = UserData} = State) ->
	% performs callback and get results
	{http_request, Method, _, _} = Request,
	Reply = dispatch(Method, Request, Headers, Body, Callback, UserData),
	gen_tcp:send(State#state.socket, Reply),
	State.

dispatch('GET', Request, Headers, _Body, Callback, UserData) ->
	Callback:get(Request, Headers, UserData);
dispatch('DELETE', Request, Headers, _Body, Callback, UserData) ->
	Callback:delete(Request, Headers, UserData);
dispatch('HEAD', Request, Headers, _Body, Callback, UserData) ->
	Callback:head(Request, Headers, UserData);
dispatch('POST', Request, Headers, Body, Callback, UserData) ->
	Callback:post(Request, Headers, Body, UserData);
dispatch('PUT', Request, Headers, Body, Callback, UserData) ->
	Callback:put(Request, Headers, Body, UserData);
dispatch('TRACE', Request, Headers, Body, Callback, UserData) ->
	Callback:trace(Request, Headers, Body, UserData);
dispatch('OPTIONS', Request, Headers, Body, Callback, UserData) ->
	Callback:options(Request, Headers, Body, UserData);
dispatch(_Other, Request, Headers, Body, Callback, UserData) ->
	Callback:other_methods(Request, Headers, Body, UserData).
