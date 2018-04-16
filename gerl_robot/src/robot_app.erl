-module(robot_app).
-include ("common.hrl").
-include ("log.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("start robot app\n"),
	{ok, SupPid} = robot_sup:start_link(),
	ets:new(tab_account, [named_table, ordered_set, public, {keypos, #robot.account}]),
	start_robots(),
	{ok, SupPid}.

stop(_State) ->
    ok.

start_robots() ->
	{ok, Numbers}       = application:get_env(robot, robot_number),
	{ok, Ip}            = application:get_env(robot, ip),
	{ok, Port}          = application:get_env(robot, port),
	{ok, AccountPrefix} = application:get_env(robot, account_prefix),
	?DBG(Numbers),
	Fun = fun(Id) ->
		Account = AccountPrefix ++ integer_to_list(Id),
		{ok, _Ret} = supervisor:start_child(robot_sup, [Ip, Port, Account]),
		timer:sleep(500)
	end,
	[Fun(N) || N <- lists:seq(1, Numbers)],
	ok.
