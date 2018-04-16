%% @author dzR <dizengrong@gmail.com>
%% @doc 地图中的字典数据

-module (map_dict).
-include ("map.hrl").
-export ([init/1]).
-export ([get_map_size/1, set_map_size/3]).
-export ([set_cell_objects/3, get_cell_objects/2]). 
-export ([add_object_to_cell/3, remove_object_from_cell/3]).
-export ([set_object_cell_rec/2, get_object_cell_rec/2, delete_object_cell_rec/2]).
-export ([get_pos/2]).
-export ([get_now_seconds/0, set_now_seconds/1]).
-export ([add_in_map_role/1, remove_in_map_role/1, get_in_map_roles/0]).
%% 其他字典数据的通用操作接口
-export ([get/1, set/2, delete/1]).

%% @doc 初始化地图字典数据
init(State) ->
	MapId   = State#map_state.map_id,
	CMapRec = cfg_map:get(MapId),
	Column  = util_math:ceil_div(CMapRec#c_map.column, ?CELL_COLUMN),
	Row     = util_math:ceil_div(CMapRec#c_map.row, ?CELL_ROW),
	set_map_size(MapId, Row, Column),
	set_in_map_roles([]),
	%% 初始化每个cell中的对象，对cell中有哪些对象做索引
	[begin set_cell_objects(?OBJECT_ROLE, CellId, []), 
		   set_cell_objects(?OBJECT_NPC, CellId, []) end || CellId <- lists:seq(1, Row * Column)],
	ok.

%% 操作字典数据的通用接口
get(Key) 		-> erlang:get(Key).
set(Key, Value) -> erlang:put(Key, Value).
delete(Key) 	-> erlang:erase(Key).

set_now_seconds(Seconds) -> ?MODULE:set({?MODULE, now_seconds}, Seconds).
get_now_seconds() -> ?MODULE:get({?MODULE, now_seconds}).

%% @doc 操作地图大小数据的api
set_map_size(MapId, Row, Column) -> erlang:put({map_size, MapId}, {Row, Column}).
get_map_size(MapId) -> erlang:get({map_size, MapId}).


-spec get_pos(ObjectType::integer(), ObjectId::integer()) -> #r_pos{}. 
%% @doc 获取object的当前位置
get_pos(ObjectType, ObjectId) ->
	ObjectCellRec = get_object_cell_rec(ObjectType, ObjectId),
	case ObjectType of 
		?OBJECT_ROLE -> ObjectCellRec#r_cell_role.pos;
		?OBJECT_NPC  -> ObjectCellRec#r_cell_npc.pos
	end.


%% @doc 操作cell中有哪些object的数据，cell中的数据为: [object_id]
set_cell_objects(ObjectType, CellId, ObjectIds) -> 
	erlang:put({cell_object_id_list, ObjectType, CellId}, ObjectIds).
get_cell_objects(ObjectType, CellId) ->
	erlang:get({cell_object_id_list, ObjectType, CellId}).


-spec add_object_to_cell(ObjectType::integer(), 
						 CellId::integer(), 
						 ObjectId::integer()) -> any().
%% @doc 将object添加到cell_object_id_list中
add_object_to_cell(ObjectType, CellId, ObjectId) ->
	ObjectIds = get_cell_objects(ObjectType, CellId),
	set_cell_objects(ObjectType, CellId, [ObjectId | ObjectIds]).


-spec remove_object_from_cell(ObjectType::integer(), 
							  CellId::integer(), 
							  ObjectId::integer()) -> any().
%% @doc 将object从CellId中移除
remove_object_from_cell(ObjectType, CellId, ObjectId) ->
	ObjectIds = get_cell_objects(ObjectType, CellId),
	set_cell_objects(ObjectType, CellId, lists:delete(ObjectId, ObjectIds)).


%% @doc 操作r_cell_xxx记录的方法
-spec set_object_cell_rec(ObjectType::integer(), 
						  ObjectCellRec::tuple()) -> any(). 
set_object_cell_rec(ObjectType, ObjectCellRec) -> 
	ObjectId = case ObjectType of 
		?OBJECT_ROLE -> ObjectCellRec#r_cell_role.id;
		?OBJECT_NPC  -> ObjectCellRec#r_cell_npc.id
	end,
	erlang:put({cell_object, ObjectType, ObjectId}, ObjectCellRec).

-spec get_object_cell_rec(ObjectType::integer(), 
						  ObjectId::integer()) -> ObjectCellRec::tuple() | undefined.
get_object_cell_rec(ObjectType, ObjectId) ->
	erlang:get({cell_object, ObjectType, ObjectId}).


-spec delete_object_cell_rec(ObjectType::integer(), ObjectId::integer()) -> any().
delete_object_cell_rec(ObjectType, ObjectId) ->
	erlang:erase({cell_object, ObjectType, ObjectId}).


%% @doc 操作索引在地图中的玩家的id列表 
%% InMapRoles:[role_id()]
add_in_map_role(RoleId) ->
	set_in_map_roles([RoleId | get_in_map_roles()]).
remove_in_map_role(RoleId) ->
	set_in_map_roles(lists:delete(RoleId, get_in_map_roles())).

get_in_map_roles() -> 
	erlang:get({?MODULE, in_map_roles}).	
set_in_map_roles(InMapRoles) -> 
	erlang:put({?MODULE, in_map_roles}, InMapRoles).	

