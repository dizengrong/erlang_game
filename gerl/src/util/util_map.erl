%% @author dzR <dizengrong@gmail.com>
%% @doc 与地图相关的一些util方法

-module (util_map).
-include ("map.hrl").

-export ([is_can_move/3, move_used_tick/2]).

%% @doc 检查该地图上的某个点是否能走
is_can_move(MapId, X, Y) -> cfg_map:is_can_move(MapId, X, Y).

%% @doc 已速度MoveSpeed在地图中移动Length长度需要多少个tick
%% 这里假设移动一个长度需要50毫秒，
move_used_tick(MoveSpeed, Length) ->
	util_math:ceil((Length * 50) / MoveSpeed).
