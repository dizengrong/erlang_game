%% @author dzR <dizengrong@gmail.com>
%% @doc npc模块，非玩家控制的能活动的角色

-module (npc).
-include("log.hrl").
-include("map.hrl").
-include("npc.hrl").

-export([init/1]).

%% @doc 初始化地图中的npc
init(MapState) ->
	InitNpcList = cfg_map:npc(MapState#map_state.map_id),
	_ = create(InitNpcList, MapState),
	ok.

make_new_cell_npc(Id, NpcTypeId, PosRec) ->
	CNpcRec = cfg_npc:get(NpcTypeId),
	CellNpcRec = #r_cell_npc{
		id         = Id,
		type_id    = NpcTypeId,
		cell_id    = map_cell:cell_id(PosRec),
		pos        = PosRec,
		move_speed = CNpcRec#c_npc.move_speed
	},
	CellNpcRec.

make_new_npc(NpcTypeId, PosRec) ->
	CNpcRec = cfg_npc:get(NpcTypeId),
	NpcRec = #r_npc{
		id         = npc_dict:next_npc_id(),
		type_id    = NpcTypeId,
		state      = ?NS_FIRST_BORN,
		next_tick  = 0,
		attr       = CNpcRec#c_npc.attr,
		born_pos   = PosRec
	},
	NpcRec.
	
create([], _MapState) -> ok;
create([{NpcTypeId, X, Y} | Rest], MapState) ->
	PosRec     = #r_pos{map_id = MapState#map_state.map_id, x = X, y = Y},
	NpcRec = make_new_npc(NpcTypeId, PosRec),
	npc_dict:set_npc_rec(NpcRec),

	CellNpcRec = make_new_cell_npc(NpcRec#r_npc.id, NpcTypeId, PosRec),
	map_api:add_npc_to_cell(CellNpcRec),
	map_api:broadcast_npc_enter(CellNpcRec),
	add_to_loop(NpcRec),
	create(Rest, MapState).

%% @doc 将npc添加到循环列表中去，这样它就可以进入有限状态机模式了
add_to_loop(NpcRec) ->
	NpcIdList = [NpcRec#r_npc.id | npc_dict:get_all_npc_id_list()],
	npc_dict:set_all_npc_id_list(NpcIdList),
	ok.
