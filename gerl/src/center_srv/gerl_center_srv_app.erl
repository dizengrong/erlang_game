-module(gerl_center_srv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	%% 一些动态数据的ets放在这里来创建，避免被监控的进程被重启后ets数据也没了
    ets:new(ets_online, [public,named_table,set]),
    gerl_center_srv_sup:start_link().

stop(_State) ->
    ok.
