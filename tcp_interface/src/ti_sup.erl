%% @author user
%% @doc @todo Add description to ti_sup.


-module(ti_sup).

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

%% --------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------
start_link(LSock) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
	supervisor:start_child(?SERVER, []). % start child of this module type

%% --------------------------------------------------------------------
%% supervisor callbacks
%% --------------------------------------------------------------------
init([LSock]) ->
	% supervisor init, gets socket
	Server = {ti_server, {ti_server, start_link, [LSock]},
			  temporary, brutal_kill, worker, [ti_server]},
	% above ti_server, socket used in child spec
	Children = [Server],
	RestartStrategy = {simple_one_for_one, 0, 1},
	{ok, {RestartStrategy, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


