-module (ai_attack).
-compile([export_all]).
-include ("common.hrl").
-include ("map.hrl").

%% 攻击怪物的ai，
% ai_datas字段的格式：[ai_status | OtherDatas]
-define(AI_STATUS_FIND_MONSTER,  	1).		%% 寻找怪物
-define(AI_STATUS_ATTACK,	  		3).		%% 打怪


handle(State) ->
	WalkPath = gen_walk_path(State),
	State#robot{
		walk_path = WalkPath, 
		status    = ?ROBOT_STATUS_WALKING, 
		ai_datas  = [?AI_STATUS_FIND_MONSTER]
	}.

%% 无头苍蝇的走法
%% 随机一个方向，然后沿着这个方向走
-define(ALL_DIRS, 	[3, 7, 5, 1]).
% 3:正x轴
% 7:负x轴
% 5:正y轴
% 1:负y轴
gen_walk_path(State) ->
	MapId  = State#robot.map_id,
	Dir    = robot_util:random_from_list(?ALL_DIRS),
	#r_pos{x = X, y = Y} = State#robot.pos,
	case Dir of
		3 -> X2 = X + 10, Y2 = Y;
		7 -> X2 = X - 10, Y2 = Y;
		5 -> X2 = X, Y2 = Y + 10;
		1 -> X2 = X, Y2 = Y - 10
	end,
	{DestX, DestY} = get_walkable_pos(MapId, X2, Y2, Dir),
	DestPos = #r_pos{x = DestX, y = DestY},
	gen_walk_path(MapId, State#robot.pos, DestPos).

get_walkable_pos(MapId, X, Y, Dir) ->
	get_walkable_pos(MapId, X, Y, Dir, cfg_map:born_pos(MapId), 20).
get_walkable_pos(_MapId, _X, _Y, _Dir, BornPos, Count) when Count < 0 -> BornPos;
get_walkable_pos(MapId, X, Y, Dir, BornPos, Count) ->
	case cfg_map:is_can_move(MapId, X, Y) of
		true -> {X, Y};
		false -> 
			case Dir of
				3 -> get_walkable_pos(MapId, X - 1, Y, Dir, BornPos, Count - 1);
				7 -> get_walkable_pos(MapId, X + 1, Y, Dir, BornPos, Count - 1);
				5 -> get_walkable_pos(MapId, X, Y - 1, Dir, BornPos, Count - 1);
				1 -> get_walkable_pos(MapId, X, Y + 1, Dir, BornPos, Count - 1)
			end
	end.

gen_walk_path(MapId, SrcPos, DestPos) ->
	case mod_walk:get_walk_path(MapId, SrcPos, DestPos) of
		false -> [];
		{ok, Path} ->
			case length(Path) >= 8 of
				true  -> Path2 = lists:sublist(Path, 8);
				false -> Path2 = Path
			end,
			[{PosX, PosY} || {PosX, PosY, _} <- Path2]
	end.
	

	