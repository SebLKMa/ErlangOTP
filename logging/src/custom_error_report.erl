%% @author user
%% @doc @todo Add description to custom_error_report.


-module(custom_error_report).

-behaviour(gen_event).

%% ====================================================================
%% API functions
%% ====================================================================
-export([register_with_logger/0]).

%% ====================================================================
%% gen_event callbacks
%% ====================================================================
-export([
		 init/1,
		 handle_event/2,
		 handle_call/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3
		 ]).

-record(state, {}).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------
register_with_logger() ->
	error_logger:add_report_handler(?MODULE).

%% --------------------------------------------------------------------
%% gen_event callbacks
%% --------------------------------------------------------------------
init([]) ->
	{ok, #state{}}.

handle_event(_Event, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	Reply = ok,
	{ok, Reply, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


