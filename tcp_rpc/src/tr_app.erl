%% @author LMA
%% @doc @todo Add description to tr_app.

-module(tr_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).

start(Type, StartArgs) ->
	case tr_sup:start_link() of
		{ok, Pid} -> {ok, Pid};
		Error -> Error
	end.

stop(State) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


