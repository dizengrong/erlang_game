%% @author dzR <dizengrong@gmail.com>
%% @doc 这个定义了数据库中与成就相关的表
%% 我们使用amnesia([http://amnesia.sourceforge.net])来drive我们的mysql数据库

-module (db_table_achievement).
-include_lib ("amnesia/include/amnesia_db_def.hrl").

driver_info () -> 
	db_conf:driver_info().

tables() -> [tab_achievement].

table(tab_achievement) -> %% 成就表
	[
	 % {id, integer, [auto_increment, unique, not_null]},
	 {role_id, integer, [index, not_null]},
	 {achieve_id, integer, [index, not_null]},		%% 成就id
	 {achieve_level, integer, not_null},			%% 成就等级
	 {data, term, [not_null, {default, []}]}		%% 成就记录数据，字符串转成erlang里的tuple
	];


table(Tab) -> erlang:throw({?MODULE, error_confg, 'table/1', Tab}).