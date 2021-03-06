%% @author user
%% @doc @todo Add description to resource_discovery.


-module(discovery).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% Note: 
%% net_adm:ping/1 must be first called between nodes for nodes() to
%% return the VM's connected nodes.
%% ====================================================================
-export([
		 start_link/0,
		 add_target_resource_type/1,
		 add_local_resource/2,
		 fetch_resources/1,
		 trade_resources/0
		]).

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

-record(state, {target_resource_types,
				local_resource_tuples,
				found_resource_tuples}).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_target_resource_type(Type) ->
	gen_server:cast(?SERVER, {add_target_resource_type, Type}).

add_local_resource(Type, Instance) ->
	gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

fetch_resources(Type) ->
	gen_server:call(?SERVER, {fetch_resources, Type}).

trade_resources() ->
	gen_server:cast(?SERVER, trade_resources).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
init([]) ->
	{ok, #state{target_resource_types = [],
				local_resource_tuples = dict:new(),
				found_resource_tuples = dict:new()}}.

handle_call({fetch_resources, Type}, _From, State) ->
	{reply, dict:find(Type, State#state.found_resource_tuples), State}.

handle_cast({add_target_resource_type, Type}, State) ->
	TargetTypes = State#state.target_resource_types,
	NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
	{noreply, State#state{target_resource_types = NewTargetTypes}};
handle_cast({add_local_resource, {Type, Instance}}, State) ->
	ResourceTuples = State#state.local_resource_tuples,
	NewResourceTuples = add_resource(Type, Instance, ResourceTuples),
	{noreply, State#state{local_resource_tuples = NewResourceTuples}};
handle_cast(trade_resources, State) ->
	ResourceTuples = State#state.local_resource_tuples,
	AllNodes = [node() | nodes()],
	lists:foreach(
	  fun(Node) -> gen_server:cast({?SERVER, Node}, 
								   {trade_resources, {node(), ResourceTuples}})
	  end, 
	  AllNodes),
	{noreply, State};
handle_cast({trade_resources, {ReplyTo, Remotes}},
			#state{local_resource_tuples = Locals,
				   target_resource_types = TargetTypes,
				   found_resource_tuples = OldFound} = State) ->
	FilteredRemotes = resources_for_types(TargetTypes, Remotes),
	NewFound = add_resources(FilteredRemotes, OldFound),
	case ReplyTo of
		noreply ->
			ok;
		_ ->
			gen_server:cast({?SERVER, ReplyTo},
							{trade_resources, {noreply, Locals}})
	end,
	{noreply, State#state{found_resource_tuples = NewFound}}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
add_resource(Type, Resource, ResourceTuples) ->
	case dict:find(Type, ResourceTuples) of
		{ok, ResourceList} ->
			NewList = [Resource | lists:delete(Resource, ResourceList)],
			dict:store(Type, NewList, ResourceTuples);
		error ->
			dict:store(Type, [Resource], ResourceTuples)
	end.

add_resources([{Type, Resource}|T], ResourceTuples) ->
	add_resources(T, add_resource(Type, Resource, ResourceTuples));
add_resources([], ResourceTuples) ->
	ResourceTuples.

resources_for_types(Types, ResourceTuples) ->
	Fun = 
		fun(Type, Acc) ->
			case dict:find(Type, ResourceTuples) of
				{ok, List} ->
					[{Type, Resource} || Resource <- List] ++ Acc;
				error ->
					Acc
			end
		end,
	lists:foldl(Fun, [], Types).



