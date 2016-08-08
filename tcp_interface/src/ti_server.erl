%% @author user
%% @doc @todo Add description to ti_server.


-module(ti_server).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

%% ====================================================================
%% gen_server functions
%% ====================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-record(state, {lsock}).

start_link(LSock) ->
	gen_server:start_link(?MODULE, [LSock], []).

init(LSock) ->
	{ok, #state{lsock = LSock}, 0}. % sets 0 as timeout immediately, this make the new gen_server
									% process jumps immediately to handle_info(timeout ,...)

handle_call(Msg, _From, State) ->
	{reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
	NewState = handle_data(Socket, RawData, State), % process incoming data
	{noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State}; % must handle tcp_closed message to ensure this ti_serer process goes away
handle_info(timeout, #state{lsock = LSock} = State) -> % handles timeout here
	{ok, _Sock} = gen_tcp:accept(LSock), % blocks until next incoming TCP connection
	ti_sup:start_child(), % supervisor to immediately start a child of this ti_server to accept next connection
	{no_reply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
handle_data(Socket, RawData, State) ->
	% echoes data back on socket, testing only
	%gen_tcp:send(Socket, RawData), 
	
	try
		{Function, RawArgList} =
			lists:splitwith(fun (C) -> C =/= $[ end, RawData), % split at the first [
		{ok, Toks, _Line} = erl_scan:string(RawArgList ++ ".", 1), % use erl_scan to produce tokens
		{ok, Args} = erl_parse:parse_term(Toks),
		Result = apply(simple_cache, list_to_atom(Function), Args), % applied Function must have been exported by simple_cache
		gen_tcp:send(Socket, io_lib:fwrite("OK:~p.~n", [Result]))
	catch
		_Class:Err ->
			gen_tcp:send(Socket, io_lib:fwrite("ERROR:~p.~n", [Err]))
	end,
	State.


