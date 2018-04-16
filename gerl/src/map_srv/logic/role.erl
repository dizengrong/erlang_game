%% @author dzR <dizengrong@gmail.com>
%% @doc 与玩家相关的逻辑处理

-module (role).
-include ("map.hrl").
-include ("db_table_role.hrl").

-export([get_pos/1, save_pos/1, persistent_pos/4, make_tab_pos_rec/4]).
-export([get_level/1, get_move_speed/1]).

%% @doc 获取玩家的等级
get_level(RoleId) ->
	Rec = hd(role_dict:lookup(RoleId, tab_role_data)),
	Rec#tab_role_data.role_level.

%% @doc 获取玩家的移动速度
get_move_speed(RoleId) ->
	Rec = hd(role_dict:lookup(RoleId, tab_fight_attr)),
	Rec#tab_fight_attr.move_speed.


-spec get_pos(RoleId::integer()) -> #r_pos{}.
%% @doc 从role_dict缓存中获取玩家的当前位置
%% 注意这个方法只在玩家还未建立cell_role数据record时会调用的
get_pos(RoleId) ->
	TabPosRec = hd(role_dict:lookup(RoleId, tab_role_pos)),
	PosRec = #r_pos{
		map_id = TabPosRec#tab_role_pos.cur_mapid,
		x      = TabPosRec#tab_role_pos.cur_x,
		y      = TabPosRec#tab_role_pos.cur_y
	},
	PosRec.

-spec save_pos(RoleId::integer()) -> any().
%% @doc 将玩家位置数据保存到role_dict中
save_pos(RoleId) ->
	PosRec    = map_dict:get_pos(?OBJECT_ROLE, RoleId),
	TabPosRec = make_tab_pos_rec(RoleId, PosRec),
	role_dict:update(RoleId, tab_role_pos, TabPosRec).

-spec persistent_pos(RoleId::integer(), NewMapId::integer(), 
					 NewX::integer(), NewY::integer()) -> any().
%% @doc 直接持久化新的坐标到缓存数据库
persistent_pos(RoleId, NewMapId, NewX, NewY) ->
	TabPosRec = make_tab_pos_rec(RoleId, NewMapId, NewX, NewY),
	db_role:update_role_pos_rec(TabPosRec).

make_tab_pos_rec(RoleId, PosRec) ->
	make_tab_pos_rec(RoleId, PosRec#r_pos.map_id, PosRec#r_pos.x, PosRec#r_pos.y).
make_tab_pos_rec(RoleId, MapId, X, Y) ->
	#tab_role_pos{
		role_id   = RoleId,
		cur_mapid = MapId,
		cur_x     = X,
		cur_y     = Y
	}.

