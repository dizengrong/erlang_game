%% @doc 有关账号的一些初始数据的配置

-module (cfg_init_datas).
-compile([export_all]).

get(newer_mapid) -> 10000;	%% 新手第一次进入游戏来到的地图
get(move_speed) -> 100;		%% 初始化时的速度

get(Key) -> erlang:throw({?MODULE, error_confg, 'get/1', Key}).
