%% @author dzR <dizengrong@gmail.com>
%% @doc 从gen_cache中获取账号相关数据的接口，对应的表模块：db_table_account

-module (db_account).
-include ("db_table_account.hrl").
-compile([export_all]).

-define(CACHE_REF(Tab), cache_util:get_cache_name(Tab)).

get_account_rec(RoleId) ->
	gen_cache:lookup(?CACHE_REF(tab_account), tab_account, RoleId).

get_role_name(RoleId) ->
	gen_cache:lookup_element(?CACHE_REF(tab_account), 
			tab_account, RoleId, #tab_account.role_name).

insert_account_rec(AccountRec) ->
	gen_cache:insert(?CACHE_REF(tab_account), tab_account, AccountRec).