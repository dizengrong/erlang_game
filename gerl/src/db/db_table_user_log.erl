%% @author dzR <dizengrong@gmail.com>
%% @doc 这个定义了数据库中与用户日志相关的表
%% 我们使用amnesia([http://amnesia.sourceforge.net])来drive我们的mysql数据库

-module (db_table_user_log).
-include_lib ("amnesia/include/amnesia_db_def.hrl").

driver_info () -> 
	db_conf:driver_info().

tables() -> [tab_log_gold, tab_log_item].

table(tab_log_gold) ->
	[{role_id, integer, [unique, not_null]},
	 {old_gold, integer, [not_null, {default, 0}]},	 	%% 原有的元宝
	 {new_gold, integer, [not_null, {default, 0}]},	 	%% 现有的元宝
	 {in_or_use, integer, [not_null, {default, 1}]}, 	%% 收入还是支出(1:收入, 2:支出)
	 {log_descript, varchar, [not_null, {default, ""}]}  %% 日志说明
	];

table(tab_log_item) ->
	[{role_id, integer, [unique, not_null]},
	 {item_typeid, integer, [not_null, {default, 0}]},	 %% 物品类型id
	 {item_name, varchar, [not_null, {default, ""}]},	 %% 物品名称
	 {log_descript, varchar, [not_null, {default, ""}]}  %% 日志说明
	];

table(Tab) -> erlang:throw({?MODULE, error_confg, 'table/1', Tab}).