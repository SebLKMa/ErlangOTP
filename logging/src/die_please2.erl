%% @author user
%% @doc @todo Add description to die_please2.


-module(die_please2).


%% ====================================================================
%% This module does not use OTP or SASL.
%% Just the direct way to crash.
%% No supervisor, no error logging.
%% ====================================================================
-export([go/0]).

-define(SLEEP_TIME, 2000).

go() ->
	%% just sleep and then crash
	timer:sleep(?SLEEP_TIME),
	i_really_want_to_die = right_now.




