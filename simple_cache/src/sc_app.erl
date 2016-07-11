%% @author LMA
%% @doc @todo Add description to sc_app.

-module(sc_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	case sc_sup:start_link() of
		{ok, Pid} -> {ok, Pid};
	Error ->
		{Error}
	end.

stop(_State) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

