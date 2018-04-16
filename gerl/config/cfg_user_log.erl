%% @doc 用户日志的配置

-module (cfg_user_log).
-compile([export_all]).

%% @doc 获取LogType对应的日志描述
%% 说明：如果介意直接硬编码数字到程序中，可以改为使用erlang的atom来替代

%% 10000 -- 14999:元宝增加的log
get_descript(10000) -> <<"登陆奖励">>;

%% 15000 -- 19999:元宝减少的log
get_descript(15000) -> <<"购买坐骑">>;

%% 20000 -- 24999:获得物品的log
get_descript(20000) -> <<"登陆奖励">>;

%% 25000 -- 29999:物品使用的log
get_descript(25000) -> <<"强化失去">>;


get_descript(LogType) -> io_lib:format(<<"~w:未添加描述">>, [LogType]).