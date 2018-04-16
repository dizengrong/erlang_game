%% @author dzR <dizengrong@gmail.com>
%% @doc 这个定义了数据库中与玩家hero相关的表
%% 我们使用amnesia([http://amnesia.sourceforge.net])来drive我们的mysql数据库

-module (db_table_role).
-include ("bag.hrl").
-include_lib ("amnesia/include/amnesia_db_def.hrl").

driver_info () -> 
	db_conf:driver_info().

tables() -> [tab_fight_attr, tab_troops, tab_role_data, tab_role_pos, tab_role_chat].

table(tab_fight_attr) -> %% 战斗属性表(属性数据只保存需要保存的，配置的一部分不包括的)
	[{role_id, integer, [unique, not_null]},
	 {hero_id, integer, not_null},							%% 英雄id
	 {leadership, integer, [not_null, {default, 0}]},		%% 领导力
	 {attack, integer, [not_null, {default, 0}]},			%% 攻击力
	 {defence, integer, [not_null, {default, 0}]},			%% 防御力
	 {intel, integer, [not_null, {default, 0}]},			%% 智力
	 {magic, integer, [not_null, {default, 0}]},			%% 魔法值
	 {max_magic, integer, [not_null, {default, 0}]},		%% 最大魔法值
	 {rage, integer, [not_null, {default, 0}]},				%% 狂怒值
	 {max_rage, integer, [not_null, {default, 0}]},			%% 最大狂怒值
	 {move_speed, integer, [not_null, {default, 0}]}		%% 移动速度
	];

table(tab_troops) -> %% 玩家的军队表
	[
	 % {id, integer, [auto_increment, unique, not_null]},
	 {role_id, integer, [index, not_null]},
	 {troops_id, integer, [not_null, {default, 0}]},			%% 军队类型id
	 {troops_amount, integer, [not_null, {default, 0}]},		%% 军队数量
	 {place, integer, [index, not_null, {default, 0}]},		%% 驻军地点(1-5为出战, 6-10为备用, 其他为NPC表示在城堡中)
	 {level, integer, [not_null, {default, 1}]},				%% 军队等级
	 {exp, integer, [not_null, {default, 0}]}					%% 军队经验
	];

table(tab_role_data) -> %% 玩家的一些其他数据
	[{role_id, integer, [unique, not_null]},
	 {role_level, integer, [not_null, {default, 1}]},			%% 玩家等级
	 {role_exp, integer, [not_null, {default, 0}]},				%% 玩家当前经验
	 {reserve_troops, integer, [not_null, {default, 2}]},		%% 备用军队槽位数量
	 {bag_capacity, integer, [not_null, {default, ?DEFAULT_BAG_CAPACITY}]}	%% 背包容量
	];

table(tab_role_pos) -> %% 玩家的一些位置数据
	[{role_id, integer, [unique, not_null]},
	 {cur_mapid, integer, not_null},		%% 当前所在地图数据		
	 {cur_x, integer, not_null},		
	 {cur_y, integer, not_null},
	 {dir, integer, [not_null, {default, 0}]}				%% 方向
	 % {from_mapid, integer, not_null},		%% 从哪个地方来
	 % {from_x, integer, not_null},		
	 % {from_y, integer, not_null}		
	];

table(tab_role_chat) -> %% 玩家聊天的相关数据
	[{role_id, integer, [unique, not_null]},
	 {no_speak, integer, [not_null, {default, 0}]}	%% 禁言时间，表示到何时才解除禁言，-1表示永久禁言
	];

table(Tab) -> erlang:throw({?MODULE, error_confg, 'table/1', Tab}).