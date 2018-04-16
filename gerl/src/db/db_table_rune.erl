%% @author dzR <dizengrong@gmail.com>
%% @doc 这个定义了数据库中与符石相关的表
%% 我们使用amnesia([http://amnesia.sourceforge.net])来drive我们的mysql数据库

-module (db_table_rune).
-include_lib ("amnesia/include/amnesia_db_def.hrl").

driver_info () -> 
	db_conf:driver_info().

tables() -> [tab_rune].

table(tab_rune) -> %% 符石表
	[{role_id, integer, [unique, not_null]},
	 {str_rune, integer, [not_null, {default, 0}]},		%% 拥有的力量符石
	 {will_rune, integer, [not_null, {default, 0}]},	%% 拥有的意志符石
	 {magic_rune, integer, [not_null, {default, 0}]},	%% 拥有的魔法符石

	 {str_rune_skills, term, [not_null, {default, []}]},	%% 已学习的力量符石技能(格式:"[符石技能id]")
	 {will_rune_skills, term, [not_null, {default, []}]},	%% 已学习的意志符石技能(格式:"[符石技能id]")
	 {magic_rune_skills, term, [not_null, {default, []}]}	%% 已学习的魔法符石技能(格式:"[符石技能id]")
	];

table(Tab) -> erlang:throw({?MODULE, error_confg, 'table/1', Tab}).