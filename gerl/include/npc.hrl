%% @doc npc相关定义的头文件

-record(r_npc, {
	id         = 0,		%% npc id
	type_id    = 0,		%% npc 类型id
	ai, 				%% npc的ai(#r_ai{})
	next_tick  = 0, 	%% 下一次行动的tick
	attr,				%% 战斗属性(#r_fight_attr{})
	state      = 0,		%% 状态
	born_pos			%% 出生地点
}).

%% npc的配置record
-record (c_npc, {
	type_id      = 0,	%% npc 类型id
	move_radius  = 0, 	%% 移动偏离出生点的最大半径
	guard_radius = 0, 	%% 警戒半径
	move_speed   = 0, 	%% 移动速度
	attr				%% 战斗属性(#r_fight_attr{})
}).

%% npc的fsm状态定义：
-define(NS_FIRST_BORN,	1).		%% 第一次出生
-define(NS_IDLE,		2).		%% 空闲
