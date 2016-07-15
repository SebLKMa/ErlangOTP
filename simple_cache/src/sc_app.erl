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
		{ok, Pid} ->
			sc_event_logger:add_handler(), % starts event logging
			{ok, Pid};
		Other -> {error, Other}
	end.

stop(_State) ->
	sc_event_logger:delete_handler(), % stops event logging
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

