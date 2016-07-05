%% @author LMA
%% @doc @todo Add description to tr_sup.

-module(tr_sup).

-behaviour(supervisor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	Server = {tr_server, {tr_server, start_link, []},
			  permanent, 2000, worker, [tr_server]},
	{ok, {{one_for_one, 0, 1}, [Server]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================



