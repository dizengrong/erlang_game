%% @doc 有关地图的一些定义

-define(MAIN_LOOP_INTERVAL, 200).	%% 地图主循环的时间间隔(毫秒)

%% 地图类型定义
-define (MAP_TYPE_CITY, 	10).	%% 城镇
-define (MAP_TYPE_SINGLE, 	11).	%% 单人副本
-define (MAP_TYPE_TEAM, 	12).	%% 组队副本
-define (MAP_TYPE_MULTI, 	13).	%% 多人大型副本

%% 九宫格的一个cell的列宽和行高
-define(CELL_COLUMN, 	12).	%% 列宽
-define(CELL_ROW,  		12).	%% 行高

%% 从一个点移动到另一个点的最长距离，该值必须小于?CELL_COLUMN和?CELL_ROW中的任何一个
-define(MAX_MOVE_LEN,   	10).	
%% 移动check路径上的一个点时，该点不能偏离路径上的点的范围
-define(MOVE_CHECK_SCOPE,   8).

%% 地图中的对象类型
-define(OBJECT_ROLE, 		1).		%% 玩家
-define(OBJECT_NPC, 		2).		%% NPC

-record (map_state, {
	map_id,		 %% 地图id
	reg_name,	 %% 地图进程注册名称
	tick 		 %% 地图进程启动后主循环的计数器，一个tick时长为?MAIN_LOOP_INTERVAL
}).

%% @doc 地图配置record
-record(c_map, {
	id           = 0,	%% 地图id
	type         = 0,	%% 场景类型，1：城镇地图， 2：剧情地图， 3：副本地图
	row          = 0,	%% 客户端将地图划分了多少行
	column       = 0,	%% 客户端将地图划分了多少列				
	max_enter_lv = 0,	%% 进入的最高等级限制，0表示无限制
	min_enter_lv = 0,	%% 进入的最低等级限制，0表示无限制
	max_times    = 0	%% 进入次数限制，0表示无限制
}).	

%% @doc 地图中对象的位置record
-record (r_pos, {
	map_id = 0,		%% 地图id
	x      = 0,		%% x坐标
	y      = 0,		%% y坐标
	dir    = 0		%% 方向
}).

%% @doc 地图中cell的玩家数据
-record(r_cell_role, {
	id         = 0,	%% 玩家role_id(int)
	cell_id    = 0,	%% 所在cell的id(int)
	type_id    = 0,	%% 英雄id(int)
	name       = "",	%% 名称(string)
	level      = 0,	%% 等级(int)
	pos,			%% 位置(#r_pos{})
	title      = 0,	%% 称号(int)
	path       = [],	%% 行走路线[{X, Y}]
	state      = 0,	%% 状态(int)
	st_data    = [],	%% 与状态相关的数据([{key, value}])
	buffs      = [],	%% buff([#r_buff{}])
	move_speed = 0		%% 移动速度
}).

%% @doc 地图中cell的npc数据
-record(r_cell_npc, {
	id         = 0,	%% npc id(int)
	cell_id    = 0,	%% 所在cell的id(int)
	type_id    = 0,	%% npc 类型id(int)
	pos,			%% 位置(#r_pos{})
	path       = [],	%% 行走路线[{X, Y}]
	state      = 0,	%% 状态(int)
	st_data    = [],	%% 与状态相关的数据([{key, value}])
	buffs      = [],	%% buff([#r_buff{}])
	move_speed = 0		%% 移动速度
}).

