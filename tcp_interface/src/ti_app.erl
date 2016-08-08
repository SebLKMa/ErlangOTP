%% @author user
%% @doc @todo Add description to ti_app.


-module(ti_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1155).

start(_StartType, StartArgs) ->
	Port = case application:get_env(tcp_interface, port) of
			{ok, P} -> P;
			undefined -> ?DEFAULT_PORT
		   end,
	% creates listening socket, active mode means all incoming data 
	% will be delivered to the process which owns the socket
	% i.e, the handler that accepted the connection, 
	% see ti_server:handle_info/2
	{ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
	case ti_sup:start_link(LSock) of
		{ok, Pid} ->
			ti_sup:start_child(),	% spawns initial handler
			{ok, Pid};
		Other ->
			{error, Other}
	end.

stop(_State) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


