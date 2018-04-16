%% @author dzR <dizengrong@gmail.com>
%% @doc 这个定义了数据库中与魔法相关的表
%% 我们使用amnesia([http://amnesia.sourceforge.net])来drive我们的mysql数据库

-module (db_table_magic).
-include_lib ("amnesia/include/amnesia_db_def.hrl").

driver_info () -> 
	db_conf:driver_info().

tables() -> [tab_magic_data, tab_magic_book].

table(tab_magic_data) -> %% 魔法
	[{role_id, integer, [unique, not_null]},
	 {max_scroll, integer, [not_null, {default, 10}]},		%% 最大卷轴数
	 {magic_crystal, integer, [not_null, {default, 0}]}	%% 魔法水晶
	];

table(tab_magic_book) -> %% 魔法书柜上的魔法
	[
	 % {id, integer, [auto_increment, unique, not_null]},
	 {role_id, integer, [index, not_null]},
	 {magic_id, integer, [index, not_null, {default, 0}]},	%% 魔法id(玩家内该数据的唯一id)
	 {book_id, integer, [not_null, {default, 0}]},			%% 魔法书typeid
	 {magic_level, integer, [not_null, {default, 0}]},		%% 魔法等级
	 {has_learned, bool, [{default, false}]}				%% 是否已学习了
	];

table(Tab) -> erlang:throw({?MODULE, error_confg, 'table/1', Tab}).