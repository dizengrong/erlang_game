%% @author dzR <dizengrong@gmail.com>
%% @doc 与随机相关的一些方法

-module (util_random).
-export([random_between/2]).

-spec random_between(Min::integer(), Max::integer()) -> integer().
%% @doc从区间[Min, Max]随机一个整形
random_between(Min, Max) ->
	check_and_set_seed(),
	Min2 = Min - 1,
	random:uniform(Max - Min2) + Min2.

check_and_set_seed() ->
	case erlang:get(my_random_seed) of
		undefined -> 
			random:seed(now()),
			erlang:put(my_random_seed, true);
		_ -> ok
	end.
