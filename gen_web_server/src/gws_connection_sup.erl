%% @author user
%% @doc 
%% This is a simple-one-for-one supervisor
%% A gws_connection_sup process will be created everytime
%% a gen_web_server instance is started to listen on a
%% particular port.

-module(gws_connection_sup).

-behaviour(supervisor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4, start_child/1]).

%% ====================================================================
%% Supervisor callbacks
%% ====================================================================
-export([init/1]).

%% ====================================================================
%% API functions
start_link(Callback, IP, Port, UserArgs) ->
	{ok, Pid} = supervisor:start_link(?MODULE, [Callback, IP, 
												Port, UserArgs]),
	start_child(Pid), % starts the first gws_server child
	{ok, Pid}.

start_child(Server) ->
	supervisor:start_child(Server, []).

%% ====================================================================
%% Supervisor callbacks
init([Callback, IP, Port, UserArgs]) ->
	BasicSockOpts = [binary,		     % not strings
					 {active, false},    % passive, this influences message reads
					 {packet, http_bin}, % incoming expected to be HTTP formatted
					 {reuseaddr, true}], % local port numbers to be reused asap
	SockOpts = case IP of
					undefined -> BasicSockOpts;
				    _		  -> [{ip, IP} | BasicSockOpts]
			   end,
	{ok, LSock} = gen_tcp:listen(Port, SockOpts), % listening socket must be owned
                                                  % by the process that stays alive.
                                                  % At this point, gws_server 
                                                  % instance is not yet started.
                                                  % 
	Server = {gws_server, {gws_server, start_link,
						   [Callback, LSock, UserArgs]},
			  temporary, brutal_kill, worker, [gws_server]},
	RestartStrategy = {simple_one_for_one, 1000, 3600},
	{ok, {RestartStrategy, [Server]}}.


%% ====================================================================
%% Internal functions
%% ====================================================================


