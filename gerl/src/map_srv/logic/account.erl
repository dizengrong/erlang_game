%% @author dzR <dizengrong@gmail.com>
%% @doc 与账号表数据相关的逻辑处理

-module (account).
-include ("db_table_account.hrl").

-export([get_role_name/1, get_account_name/1]).

%% @doc 获取玩家的角色名
get_role_name(RoleId) ->
	[Rec | _] = role_dict:lookup(RoleId, tab_account),
	Rec#tab_account.role_name.

%% @doc 获取玩家的账号名
get_account_name(RoleId) ->
	[Rec | _] = role_dict:lookup(RoleId, tab_account),
	Rec#tab_account.account.
