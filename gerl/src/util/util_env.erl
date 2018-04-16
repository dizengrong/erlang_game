%% @author dzR <dizengrong@gmail.com>
%% @doc 有关application env 的一些方法模块

-module (util_env).

-export ([get_env/2]).

-spec get_env(App::atom(), Key::atom()) -> any().
%% @doc 获取应用App内的Key对应的环境变量
get_env(App, Key) ->
	{ok, Val} = application:get_env(App, Key),
	Val.
