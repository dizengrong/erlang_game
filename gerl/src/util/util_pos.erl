%% @author dzR <dizengrong@gmail.com>
%% @doc 与坐标位置相关的一些util方法

-module (util_pos).
-export ([is_in_range/5]).

-spec is_in_range(X1::integer(), Y1::integer(), 
				  X2::integer(), Y2::integer(), 
				  Distance::integer()) -> boolean().
%% @doc 判断点(X1, Y1)与点(X2, Y2)的距离是否在Distance之内
is_in_range(X1, Y1, X2, Y2, Distance) ->
	(abs(X1 - X2) =< Distance andalso abs(Y1 - Y2) =< Distance).