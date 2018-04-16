%% @author dzR <dizengrong@gmail.com>
%% @doc 地图进程中的的事件回调处理

-module (map_event).
-include("map.hrl").
-include("log.hrl").

-export([init/1, role_enter/1, role_leave/1]).
-export([main_loop/1, one_second_loop/2]).

%% @doc 地图创建后的初始化回调
init(MapState) -> 
	MapModel = cfg_map_model:get_model(MapState#map_state.map_id),
	MapModel:map_event(init, MapState),
	MapState.

-spec role_enter(RoleId::integer()) -> any().
%% @doc 玩家进入地图后的事件
role_enter(RoleId) ->
	role_dict:init(RoleId),
	role_dict_tmp:init(RoleId),
	role_bag_dict:init(RoleId),
	map_dict:add_in_map_role(RoleId),
	ok.

-spec role_leave(CellRoleRec::#r_cell_role{}) -> any().
%% @doc 玩家离开所在地图之前的事件
role_leave(CellRoleRec) ->
	RoleId = CellRoleRec#r_cell_role.id,
	map_dict:remove_in_map_role(RoleId),
	%% 1.保存数据
	role:save_pos(RoleId),
	%% 2.清理玩家的地图数据
	CellId = CellRoleRec#r_cell_role.cell_id,
	map_dict:remove_object_from_cell(?OBJECT_ROLE, CellId, RoleId),
	map_dict:delete_object_cell_rec(?OBJECT_ROLE, RoleId),
	%% 3.清理玩家的字典数据
	role_dict_tmp:delete(RoleId),
	role_dict:delete(RoleId),
	role_bag_dict:delete(RoleId),
	ok.

%% @doc 地图主循环，间隔为?MAIN_LOOP_INTERVAL
%% Tick参数详见#map_state.tick字段说明
main_loop(Tick) ->
	try
		npc_fsm:main_loop(Tick)
	catch
		Type:Reason -> ?PRINT_STACKTRACE(main_loop, Type, Reason)
	end.

%% @doc 地图的每秒循环
one_second_loop(_MapState, NowSeconds) ->
	try
		msg_delay:one_second_loop(),
		_ = [role_loop:one_second_loop(RoleId, NowSeconds) 
			 || RoleId <- map_dict:get_in_map_roles()],
		ok
	catch
		Type:Reason -> ?PRINT_STACKTRACE(one_second_loop, Type, Reason)
	end.
