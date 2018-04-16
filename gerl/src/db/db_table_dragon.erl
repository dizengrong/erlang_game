%% @author dzR <dizengrong@gmail.com>
%% @doc 这个定义了数据库中与龙宝宝相关的表
%% 我们使用amnesia([http://amnesia.sourceforge.net])来drive我们的mysql数据库

-module (db_table_dragon).
-include_lib ("amnesia/include/amnesia_db_def.hrl").

driver_info () -> 
	db_conf:driver_info().

tables() -> [tab_dragon].

table(tab_dragon) -> %% 战斗属性表
	[{role_id, integer, [unique, not_null]},
	 {dragon_id, integer, not_null},				%% 龙宝宝id
	 {level, integer, [not_null, {default, 0}]},	%% 等级
	 {exp, integer, [not_null, {default, 0}]},		%% 经验
	 {magic, integer, [not_null, {default, 0}]},	%% 魔法值
	 {rage, integer, [not_null, {default, 0}]}		%% 狂怒值
	];


table(Tab) -> erlang:throw({?MODULE, error_confg, 'table/1', Tab}).