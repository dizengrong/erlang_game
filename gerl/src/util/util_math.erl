%% @author dzR <dizengrong@gmail.com>
%% @doc 和数学运算相关的方法

-module (util_math).

-export([ceil/1, floor/1, ceil_div/2]).

%% @doc 向上取整
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%% @doc 向下取整
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

%% @doc 对A除以B的商向上取整
ceil_div(A, B) ->
	C = A div B,
	case A rem B == 0 of
		true -> C;
		false -> C + 1
	end.
