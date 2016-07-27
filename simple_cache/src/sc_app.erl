%% @author LMA
%% @doc 
%% To run the code, start a cluster of 2 contact nodes in separate consoles,
%% see ensure_contact/0 for the matching node names
%% $ erl -sname contact1
%% $ erl -sname contact2
%% OR
%% $ erl -name contact1
%% $ erl -name contact2
%% Then start Erlang as follows
%% $ erl -name mynode -pa ./simple_cache/ebin -pa ./resource_discovery/ebin
%% OR if using config file in ebin
%% $ erl -name mynode -config ./simple_cache/ebin/simple_cache.config -pa ./simple_cache/ebin -pa ./resource_discovery/ebin
%% > application:start(sasl).
%% > mnesia:start().
%% > application:start(resource_discovery).
%% > application:start(simple_cache).

-module(sc_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, stop/1]).

-define(WAIT_FOR_RESOURCES, 2500).

start(_StartType, _StartArgs) ->
	% asserts known nodes are up and running
	ok = ensure_contact(),
	% what I have
	resource_discovery:add_local_resource(simple_cache, node()),
	% what I want
	resource_discovery:add_target_resource_type(simple_cache),
	% trade resources with other nodes in cluster
	resource_discovery:trade_resources(),
	timer:sleep(?WAIT_FOR_RESOURCES),
	sc_store:init(),
	case sc_sup:start_link() of
		{ok, Pid} ->
			sc_event_logger:add_handler(), % starts event logging
			{ok, Pid};
		Other -> {error, Other}
	end.

stop(_State) ->
	sc_event_logger:delete_handler(), % stops event logging
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
ensure_contact() ->
	% configure the hostname in env section of app file.
	%DefaultNodes = ['contact1@users-MacBook-Pro', 'contact2@users-MacBook-Pro'],
	DefaultNodes = ['contact1@localhost', 'contact2@localhost'],
	% checks config for nodes
	case get_env(simple_cache, contact_nodes, DefaultNodes) of
		[] ->
			{error, no_contact_nodes};
		ContactNodes ->
			ensure_contact(ContactNodes)
	end.

ensure_contact(ContactNodes) ->
	% pings the listed nodes
	Responding = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],
	case Responding of
		[] ->
			{error, no_contact_nodes_reachable};
		_ ->
			DefaultTime = 6000,
			% checks config for time to wait
			WaitTime = get_env(simple_cache, wait_time, DefaultTime),
			wait_for_nodes(length(Responding), WaitTime)
	end.

wait_for_nodes(MinNodes, WaitTime) ->
	Slices = 10,
	SliceTime = round(WaitTime/Slices),
	wait_for_nodes(MinNodes, SliceTime, Slices). % enters the wait loop

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
	ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
	case length(nodes()) > MinNodes of	% checks if enough nodes are connected
		true ->
			ok;
		false ->
			timer:sleep(SliceTime),
			wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
	end.

get_env(AppName, Key, Default) ->
	% lookup configuration data
	case application:get_env(AppName, Key) of
		undefined -> Default;
		{ok, ConfigValue} -> ConfigValue
	end.

  
