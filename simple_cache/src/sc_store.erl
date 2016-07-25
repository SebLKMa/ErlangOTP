%% @author LMA
%% @doc @todo Add description to sc_store.

-module(sc_store).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 init/0,
		 insert/2,
		 delete/1,
		 lookup/1]).

-define(TABLE_ID, ?MODULE).
-define(WAIT_FOR_TABLES, 5000).

%% mnesia record
-record(key_to_pid, {key, pid}).

%% ets impl
%init() ->
%	ets:new(?TABLE_ID, [public, named_table]),
%	ok.

%% mnesia impl 1
%% erl must be started with mnesia path, schema must have already created
%% '"/Users/user/Documents/workspace/mars/github/ErlangOTP/mnesia_storage"'
%init() ->
%	mnesia:start(),
%	mnesia:create_table(key_to_pid,		% table name to create
%						[{index, pid},	% index to create
%						 {attributes, record_info(fields, key_to_pid)}]).

%% mnesia impl 2
%% erl must be started with mnesia path, schema must have already created
%% '"/Users/user/Documents/workspace/mars/github/ErlangOTP/mnesia_storage"'
init() ->
	mnesia:stop(),
	mnesia:delete_schema([node()]),
	mnesia:start(),
	% fetch all simple_cache instances from resource_discovery
	{ok, CacheNodes} = resource_discovery:fetch_resources(simple_cache),
	% remove self instance
	dynamic_db_init(lists:delete(node(), CacheNodes)).


%% ets impl
%insert(Key, Pid) ->
%	ets:insert(?TABLE_ID, {Key, Pid}).

%% mnesia impl
insert(Key, Pid) ->
	mnesia:dirty_write(#key_to_pid{key = Key, pid = Pid}).

%% ets impl
%lookup(Key) ->
%	case ets:lookup(?TABLE_ID, Key) of
%		[{Key, Pid}] -> {ok, Pid};
%		[] -> {error, not_found}
%	end.

%% mnesia impl
lookup(Key) ->
	case mnesia:dirty_read(key_to_pid, Key) of
		[{key_to_pid, Key, Pid}] -> 
			case is_pid_alive(Pid) of 
				true -> {ok, Pid};
				false -> {error, not_found}
			end;
		[] -> 
			{error, not_found}
	end.

%% ets impl
%delete(Pid) ->
%	ets:match_delete(?TABLE_ID, {'_', Pid}).

% mnesia impl
delete(Pid) ->
	case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
		[#key_to_pid{} = Record] ->
			mnesia:dirty_delete_object(Record);
		_ ->
			ok
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
is_pid_alive(Pid) when node(Pid) =:= node() ->
	is_process_alive(Pid); % see http://erlang.org/doc/man/erlang.html#is_process_alive-1
is_pid_alive(Pid) ->
	lists:member(node(Pid), nodes()) andalso
		(rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true).

dynamic_db_init([]) ->
	mnesia:create_table(key_to_pid,		% table name to create
						[{index, pid},	% index to create
						 {attributes, record_info(fields, key_to_pid)}]);
dynamic_db_init(CacheNodes) ->
	add_extra_nodes(CacheNodes).

add_extra_nodes([Node|T]) ->
	case mnesia:change_config(extra_db_nodes, [Node]) of
		{ok, [Node]} ->
			% replaces local schema with remote
			mnesia:add_table_copy(schema, node(), ram_copies),
			mnesia:add_table_copy(key_to_pid, node(), ram_copies),
			% get a list of all existing tables
			Tables = mnesia:system_info(tables),
			% wait for newly added table to be synchronized
			mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES);
		_ ->
			add_extra_nodes(T) % tries some other nodes instead
	end.

