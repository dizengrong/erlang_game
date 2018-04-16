%% @author dzR <dizengrong@gmail.com>
%% @doc 地图数据处理提供的一个api

-module (map_api).
-include("log.hrl").
-include("map.hrl").
-include("proto_map_pb.hrl").

-export([make_new_cell_role/1, add_role_to_cell/1, broadcast_role_enter/1]).
-export([role_jump_to/4, role_move_path/2, role_move_check/3, role_leave/1]).
-export([add_npc_to_cell/1, broadcast_npc_enter/1]).
-export([broadcast_npc_move/1, npc_move_check/2]).

-spec make_new_cell_role(RoleId::integer()) -> #r_cell_role{}.
%% @doc 根据玩家的数据创建他在地图cell数据
make_new_cell_role(RoleId) ->
	PosRec = role:get_pos(RoleId),
	#r_cell_role{
		id         = RoleId,
		cell_id    = map_cell:cell_id(PosRec),
		type_id    = ?OBJECT_ROLE,
		name       = account:get_role_name(RoleId),
		level      = role:get_level(RoleId),
		pos        = PosRec,
		title      = 0,
		path       = [],
		state      = 0,
		st_data    = [],
		buffs      = [],
		move_speed = role:get_move_speed(RoleId)
	}.

-spec add_role_to_cell(CellRoleRec::#r_cell_role{}) -> any().
%% @doc 将玩家添加到cell中
add_role_to_cell(CellRoleRec) ->
	map_dict:set_object_cell_rec(?OBJECT_ROLE, CellRoleRec),
	CellId = CellRoleRec#r_cell_role.cell_id,
	map_dict:add_object_to_cell(?OBJECT_ROLE, CellId, CellRoleRec#r_cell_role.id).

-spec add_npc_to_cell(CellNpcRec::#r_cell_npc{}) -> any().
%% @doc 将NPC添加到cell中
add_npc_to_cell(CellNpcRec) ->
	map_dict:set_object_cell_rec(?OBJECT_NPC, CellNpcRec),
	CellId = CellNpcRec#r_cell_npc.cell_id,
	map_dict:add_object_to_cell(?OBJECT_NPC, CellId, CellNpcRec#r_cell_npc.id).


-spec broadcast_npc_enter(CellNpcRec::#r_cell_npc{}) -> any().
%% @doc 向玩家所在cell的九宫格范围广播他的进入
broadcast_npc_enter(CellNpcRec) ->
	MapId  = CellNpcRec#r_cell_npc.pos#r_pos.map_id,
	CellId = CellNpcRec#r_cell_npc.cell_id,
	NotifyCells = map_cell:get_9_cells_by_cell(MapId, CellId),
	broadcast_npc_enter(CellNpcRec, NotifyCells).

broadcast_npc_enter(CellNpcRec, NotifyCells) ->
	Msg = #mc_map_enter{npcs = [CellNpcRec]},
	[begin 
		CellRoleIds = map_dict:get_cell_objects(?OBJECT_ROLE, EnterCellId),
		[util_packet:send_to_role(R, Msg) || R <- CellRoleIds]
	 end || EnterCellId <- NotifyCells],
	ok.

-spec broadcast_role_enter(CellRoleRec::#r_cell_role{}) -> any().
%% @doc 向玩家所在cell的九宫格范围广播他的进入
broadcast_role_enter(CellRoleRec) ->
	MapId       = CellRoleRec#r_cell_role.pos#r_pos.map_id,
	CellId      = CellRoleRec#r_cell_role.cell_id,
	NotifyCells = map_cell:get_9_cells_by_cell(MapId, CellId),
	broadcast_role_enter(CellRoleRec, NotifyCells).

-spec broadcast_role_enter(CellRoleRec::#r_cell_role{}, 
						   NotifyCells::[integer()]) -> any().
%% @doc 向指定的cell范围广播玩家的进入
broadcast_role_enter(CellRoleRec, NotifyCells) ->
	RoleId      = CellRoleRec#r_cell_role.id,
	%% 1.通知其他玩家自己进入他们的视野了
	MsgToOthers = #mc_map_enter{roles = [CellRoleRec]},
	L = [begin 
		    CellRoleIds = map_dict:get_cell_objects(?OBJECT_ROLE, EnterCellId),
		    [begin util_packet:send_to_role(R, MsgToOthers),
		   		  map_dict:get_object_cell_rec(?OBJECT_ROLE, R)
		   	 end || R <- CellRoleIds, R /= RoleId]
	 	 end || EnterCellId <- NotifyCells],
	%% 2.通知自己哪些其他玩家进入视野了
	MsgToSelf = #mc_map_enter{
		roles = lists:flatten(L),
		npcs  = []
	},
	util_packet:send_to_role(RoleId, MsgToSelf),
	ok.


-spec broadcast_role_leave(CellRoleRec::#r_cell_role{}) -> any().
%% @doc 向玩家所在cell的九宫格范围广播他的离开
broadcast_role_leave(CellRoleRec) ->
	MapId       = CellRoleRec#r_cell_role.pos#r_pos.map_id,
	CellId      = CellRoleRec#r_cell_role.cell_id,
	NotifyCells = map_cell:get_9_cells_by_cell(MapId, CellId),
	broadcast_role_leave(CellRoleRec, NotifyCells, false),
	ok.
-spec broadcast_role_leave(CellRoleRec::#r_cell_role{}, 
						   NotifyCells::[integer()], 
						   IsNotifySelf::boolean()) -> ok.
%% @doc 向指定的cell范围广播玩家的离开
%% 参数IsNotifySelf为true表示要向该玩家发送其他玩家离开自己视野的消息
%% 为false则不需要
broadcast_role_leave(CellRoleRec, NotifyCells, IsNotifySelf) ->
	RoleId      = CellRoleRec#r_cell_role.id,
	MsgToOthers = #mc_map_leave{roles = [RoleId]},
	L = [begin CellRoleIds = map_dict:get_cell_objects(?OBJECT_ROLE, LeaveCellId),
		   [begin util_packet:send_to_role(R, MsgToOthers), R end 
		    || R <- CellRoleIds, R /= RoleId]
	 end || LeaveCellId <- NotifyCells],
	case IsNotifySelf of
		true ->
			MsgToSelf = #mc_map_leave{roles = lists:flatten(L)},
			util_packet:send_to_role(RoleId, MsgToSelf);
		_ -> ok
	end.

broadcast_role_move(CellRoleRec) ->
	RoleId      = CellRoleRec#r_cell_role.id,
	MapId       = CellRoleRec#r_cell_role.pos#r_pos.map_id,
	CellId      = CellRoleRec#r_cell_role.cell_id,
	Path        = CellRoleRec#r_cell_role.path,
	CellRoleIds = get_9_cell_role_ids_except(MapId, CellId, RoleId),
	Msg         = move_msg(?OBJECT_ROLE, RoleId, Path),
	[util_packet:send_to_role(R, Msg) || R <- CellRoleIds],
	ok.
broadcast_npc_move(CellNpcRec) ->
	MapId       = CellNpcRec#r_cell_npc.pos#r_pos.map_id,
	CellId      = CellNpcRec#r_cell_npc.cell_id,
	Path        = CellNpcRec#r_cell_npc.path,
	CellRoleIds = get_9_cell_role_ids(MapId, CellId),
	Msg         = move_msg(?OBJECT_NPC, CellNpcRec#r_cell_npc.id, Path),
	[util_packet:send_to_role(R, Msg) || R <- CellRoleIds],
	ok.

move_msg(ObjectType, ObjectId, Path) ->
	#mc_map_move{
		object_type = ObjectType, 
		object_id   = ObjectId, 
		path        = Path
	}.

%% @doc 跳转到目的点
role_jump_to(RoleId, DestMapId, DestX, DestY) ->
	role_leave(RoleId),
	role:persistent_pos(RoleId, DestMapId, DestX, DestY),
	Msg = #mc_map_jump{
		dest_map_id = DestMapId,
		dest_x      = DestX,
		dest_y      = DestY
	},
	util_packet:send_to_role(RoleId, Msg),
	map_srv:lock_packet(RoleId),
	{ok, MapNode, MapRegName} = ensure_map(RoleId, DestMapId),
	gateway_dispatch:change_map(RoleId, MapNode, MapRegName),
	ok.

%% @doc 玩家离开当前所在地图(离开就意味着其地图上的数据和玩家的字典数据也被清理了)
%% 注意这里只是离开，而最终离开后会到哪个位置去是不会处理的
role_leave(RoleId) ->
	CellRoleRec = map_dict:get_object_cell_rec(?OBJECT_ROLE, RoleId),
	broadcast_role_leave(CellRoleRec),
	map_event:role_leave(CellRoleRec),
	ok.

%% @doc 移动一条路径
%% Path:[x1, y1, x2, y2...]
role_move_path(RoleId, Path) ->
	CellRoleRec    = map_dict:get_object_cell_rec(?OBJECT_ROLE, RoleId),
	NewCellRoleRec = CellRoleRec#r_cell_role{path = Path},
	map_dict:set_object_cell_rec(?OBJECT_ROLE, NewCellRoleRec),
	broadcast_role_move(NewCellRoleRec),
	ok.

role_move_check(RoleId, CheckX, CheckY) ->
	CellRoleRec = map_dict:get_object_cell_rec(?OBJECT_ROLE, RoleId),
	MapId       = CellRoleRec#r_cell_role.pos#r_pos.map_id,
	CurX        = CellRoleRec#r_cell_role.pos#r_pos.x,
	CurY        = CellRoleRec#r_cell_role.pos#r_pos.y,
	case CellRoleRec#r_cell_role.path of
		[] -> ?WARNING("There is no path for check");
		[PathX, PathY | RestPath] ->
			IsCanMove = util_map:is_can_move(MapId, CheckX, CheckY), 
			IsMoveOk  = abs(CheckX - CurX) =< ?MAX_MOVE_LEN andalso 
					    abs(CheckY - CurY) =< ?MAX_MOVE_LEN,
			IsCheckOk = abs(CheckX - PathX) =< ?MOVE_CHECK_SCOPE andalso 
						abs(CheckY - PathY) =< ?MOVE_CHECK_SCOPE,
			if
				not IsCanMove -> 
					?ERROR("~p can not move", [{MapId, CheckX, CheckY}]),
					reset_role_pos(CellRoleRec);
				not IsMoveOk ->
					?ERROR("Cannot move from ~p to ~p", [{CurX, CurY}, {CheckX, CheckY}]),
					reset_role_pos(CellRoleRec);
				not IsCheckOk ->
					?ERROR("Cannot check from ~p to ~p", [{PathX, PathY}, {CheckX, CheckY}]),
					reset_role_pos(CellRoleRec);
				true ->
					role_move_check2(CellRoleRec, CheckX, CheckY, RestPath)
			end
	end.
role_move_check2(CellRoleRec, CheckX, CheckY, RestPath) ->
	PosRec    = CellRoleRec#r_cell_role.pos,
	MapId     = PosRec#r_pos.map_id,
	NewPosRec = PosRec#r_pos{x = CheckX, y = CheckY},
	OldCellId = CellRoleRec#r_cell_role.cell_id,
	NewCellId = map_cell:cell_id(NewPosRec),
	NewCellRoleRec = CellRoleRec#r_cell_role{
		pos     = NewPosRec,
		cell_id = NewCellId,
		path    = RestPath
	},
	map_dict:set_object_cell_rec(?OBJECT_ROLE, NewCellRoleRec),
	case map_cell:get_leave_and_enter_cells(MapId, OldCellId, NewCellId) of
		[] -> ok;
		{LeaveCells, EnterCells} ->
			broadcast_role_leave(CellRoleRec, LeaveCells, true),
			broadcast_role_enter(NewCellRoleRec, EnterCells)
	end.

npc_move_check(CellNpcRec, {CheckX, CheckY}) ->
	PosRec    = CellNpcRec#r_cell_npc.pos,
	MapId     = PosRec#r_pos.map_id,
	NewPosRec = PosRec#r_pos{x = CheckX, y = CheckY},
	OldCellId = CellNpcRec#r_cell_npc.cell_id,
	NewCellId = map_cell:cell_id(NewPosRec),
	NewCellNpcRec = CellNpcRec#r_cell_npc{
		pos     = NewPosRec,
		cell_id = NewCellId,
		path    = []
	},
	map_dict:set_object_cell_rec(?OBJECT_NPC, NewCellNpcRec),
	case map_cell:get_leave_and_enter_cells(MapId, OldCellId, NewCellId) of
		[] -> ok;
		{LeaveCells, EnterCells} ->
			broadcast_role_leave(CellNpcRec, LeaveCells, true),
			broadcast_role_enter(NewCellNpcRec, EnterCells)
	end.

reset_role_pos(_CellRoleRec) -> ok.

%% @doc 获取地图进程的名称，并确定地图进程已经被创建和注册了
ensure_map(RoleId, MapId) ->
	case map_dispatch:query_map_name(MapId) of
		undefined -> %% 即没有启动
			{ok, MapNode, MapRegName} = create_map(RoleId, MapId);
		{MapNode, MapRegName} -> ok
	end,
	{ok, MapNode, MapRegName}.

create_map(RoleId, MapId) ->
	CMapRec = cfg_map:get(MapId),
	{ok, MapNode, MapRegName} = case CMapRec#c_map.type of
		?MAP_TYPE_CITY   -> map_manager:create_map(MapId);
		?MAP_TYPE_SINGLE -> map_manager:create_single_map(MapId, RoleId);
		?MAP_TYPE_MULTI  -> map_manager:create_map(MapId);
		?MAP_TYPE_TEAM   -> 
			%% TODO: 获取组队的队伍id
			TeamId = 0,
			map_manager:create_team_map(MapId, TeamId)
	end,
	{ok, MapNode, MapRegName}.

%% @doc 根据CellId获取其九宫格内的玩家的id: [RoleId]
get_9_cell_role_ids(MapId, CellId) ->
	get_9_cell_role_ids_except(MapId, CellId, 0).
%% @doc 根据CellId获取除了ExceptId的其九宫格内的玩家的id: [RoleId]
get_9_cell_role_ids_except(MapId, CellId, ExceptId) ->
	CellIdLists = map_cell:get_9_cells_by_cell(MapId, CellId),
	Fun = fun(C, Acc) ->
		L = case ExceptId of
			0 -> map_dict:get_cell_objects(?OBJECT_ROLE, C);
			_ -> [R || R <- map_dict:get_cell_objects(?OBJECT_ROLE, C), R /= ExceptId]
		end,
		L ++ Acc
	end,
	lists:foldl(Fun, [], CellIdLists).
