%% @author LMA
%% @doc @todo Add description to hi_app.

-module(hi_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1156).

%% ====================================================================
%% API functions
start(_StartType, _StartArgs) ->
    Port = case application:get_env(http_interface, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    case hi_sup:start_link(Port) of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


