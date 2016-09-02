%% @author LMA
%% @doc @todo Add description to hi_sup.

-module(hi_sup).

-behaviour(supervisor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, start_child/0]).

%% ====================================================================
%% supervisor callbacks
%% ====================================================================
-export([init/1]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% API functions
start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

start_child() ->
    supervisor:start_child(?SERVER, []).

%% ====================================================================
%% supervisor callbacks
init([Port]) ->
    Server = {hi_server, {hi_server, start_link, [Port]},
              permanent, 2000, worker, [hi_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


