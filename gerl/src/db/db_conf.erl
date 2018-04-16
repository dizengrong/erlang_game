%% @author dzR <dizengrong@gmail.com>
%% @doc 有关数据库的一些配置

-module (db_conf).
-include ("db.hrl").
-compile([export_all]).

driver_info () ->
	[{driver, mysql_drv},
	 {host, ?DB_HOST},
	 {user, ?DB_USER},
	 {password, ?DB_PASS}].
	 