%% @author LMA
%% @doc @todo Add description to sc_app.

-module(sc_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	sc_store:init(),
	case sc_sup:start_link() of
		{ok, Pid} -> {ok, Pid};
		Other -> {error, Other}
	end.

stop(_State) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

