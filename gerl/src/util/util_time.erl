%% @author dzR <dizengrong@gmail.com>
%% @doc 有关时间的一些方法模块

-module (util_time).
-export([now/0, now_seconds/0, now_milliseconds/0]).

-spec now() -> {MegaSecs::integer(), Secs::integer(), MicroSecs::integer()}.
%% @doc 获取now时间，返回值同erlang:now()，但是这个方法更高效
now() ->
	os:timestamp().

-spec now_seconds() -> Seconds::non_neg_integer().
%% @doc 获取现在时刻的秒数
now_seconds() ->
	{A, B, _} = ?MODULE:now(),
    A * 1000000 + B.

-spec now_milliseconds() -> MilliSeconds::non_neg_integer().
%% @doc 获取现在时刻的毫秒数
now_milliseconds() ->
    {A, B, C} = ?MODULE:now(),
    A * 1000000000 + B*1000 + trunc(C/1000).
