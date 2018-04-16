%% @author dzR <dizengrong@gmail.com>
%% @doc 寻路处理

-module (path).
-include ("common.hrl").
-include ("map.hrl").
-include ("spec_type.hrl").

-export ([random_path/3, gen_path/3]).

-spec random_path(MapId::map_id(), 
				  SrcPosRec::#r_pos{}, 
				  Length::integer()) -> [{X::integer(), Y::integer()}].
%% @doc 获取一条从原点SrcPosRec开始，偏离坐标长度为Length的随机路径
random_path(MapId, SrcPosRec, Length) ->
	X = SrcPosRec#r_pos.x,
	Y = SrcPosRec#r_pos.y,
	X2 = ?_IF(util_random:random_between(1, 2) == 1, X + Length, X - Length),
	Y2 = ?_IF(util_random:random_between(1, 2) == 1, Y + Length, Y - Length),
	DestPosRec = #r_pos{x = X2, y = Y2},
	gen_path(MapId, SrcPosRec, DestPosRec).

-spec gen_path(MapId::map_id(), 
			   SrcPosRec::#r_pos{}, 
			   DestPosRec::#r_pos{}) -> [{X::integer(), Y::integer()}].
%% @doc 生成一条从SrcPosRec到DestPosRec的路径
gen_path(MapId, SrcPosRec, DestPosRec) ->
	case mod_walk:get_walk_path(MapId, SrcPosRec, DestPosRec) of
		false -> [];
		{ok, Path} -> [{X, Y} || {X, Y, _Dir} <- Path]
	end.
