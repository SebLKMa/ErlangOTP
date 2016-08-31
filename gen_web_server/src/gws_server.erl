%% @author LMA
%% @doc @todo Add description to gws_server.


-module(gws_server).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3]).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-record(state, {lsock, socket, request_line, headers = [],
				body = <<>>, content_remaining = 0,
				callback, user_data, parent}).

%% ====================================================================
%% API functions
start_link(Callback, LSock, UserArgs) ->
	gen_server:start_link(?MODULE,
						  [Callback, LSock, UserArgs, self()], []).

%% ====================================================================
%% gen_server callbacks
init([Callback, LSock, UserArgs, Parent]) ->
	{ok, UserData} = Callback:init(UserArgs),
	State = #state{lsock = LSock, callback = Callback,
				   user_data = UserData, parent = Parent},
	{ok, State, 0}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


