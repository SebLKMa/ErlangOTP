%% @author LMA
%% @doc @todo Add description to die_please.

-module(die_please).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================
-export([
		 init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3
		 ]).

-define(SERVER, ?MODULE).
-define(SLEEP_TIME, 2*1000).

-record(state, {}).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
init([]) ->
	{ok, #state{}, ?SLEEP_TIME}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{ok, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, State) ->
	i_want_to_die = right_now,
	{no_reply, State}.

terminate(_Reason, State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


