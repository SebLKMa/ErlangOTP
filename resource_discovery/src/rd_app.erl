%% @author LMA
%% @doc @todo Add description to rd_app.


-module(rd_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case rd_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions






