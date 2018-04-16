%% @author dzR <dizengrong@gmail.com>
%% @doc 记录用户日志的一些方法

-module (util_user_log).
-include ("item.hrl").
-include ("spec_type.hrl").
-include ("db_table_user_log.hrl").

-export ([add_log/1, log_item/2]).

%% @doc 增加一条日志记录
%% 参数LogRec为一个record，在db_table_user_log.erl中定义
add_log(LogRec) ->
	user_log_srv:add_log(LogRec),
	ok.

-spec log_item(RoleId::role_id(), CreateParams::#r_create_item{}) -> ok.
log_item(RoleId, CreateParams) ->
	TypeId  = CreateParams#r_create_item.type_id,
	LogType = CreateParams#r_create_item.log,
	LogRec = #tab_log_item{
		role_id      = RoleId, 
		item_typeid  = TypeId,
		item_name    = util_item:get_item_name(TypeId),
		log_descript = cfg_user_log:get_descript(LogType)
	},
	add_log(LogRec).
