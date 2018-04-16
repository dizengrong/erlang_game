%% @author dzR <dizengrong@gmail.com>
%% @doc 这个定义了数据库中与账号相关的表
%% 我们使用amnesia([http://amnesia.sourceforge.net])来drive我们的mysql数据库

-module (db_table_account).
-include_lib ("amnesia/include/amnesia_db_def.hrl").

driver_info () -> 
	db_conf:driver_info().

tables() -> [tab_account].

table(tab_account) ->
	[{role_id, integer, [unique, not_null]},
	 {account, varchar, [unique, not_null]},
	 {role_name, varchar, not_null},
	 {last_login_time, integer, [not_null, {default, 0}]}];

table(Tab) -> erlang:throw({?MODULE, error_confg, 'table/1', Tab}).