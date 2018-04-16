%% @author dzR <dizengrong@gmail.com>
%% @doc 从gen_cache中获取与玩家相关数据的接口，对应的表模块：db_table_role

-module (db_role).
-include ("db_table_role.hrl").
-compile([export_all]).

-define(CACHE_REF(Tab), cache_util:get_cache_name(Tab)).

insert_fight_attr_rec(FightAttrRec) ->
	gen_cache:insert(?CACHE_REF(tab_fight_attr), tab_fight_attr, FightAttrRec).

insert_troops_rec(TroopsRecs) ->
	gen_cache:insert(?CACHE_REF(tab_troops), tab_troops, TroopsRecs).

insert_role_data_rec(RoleDataRec) ->
	gen_cache:insert(?CACHE_REF(tab_role_data), tab_role_data, RoleDataRec).

insert_role_pos_rec(RolePosRec) ->
	gen_cache:insert(?CACHE_REF(tab_role_pos), tab_role_pos, RolePosRec).

insert_role_chat_rec(ChatRec) ->
	gen_cache:insert(?CACHE_REF(tab_role_chat), tab_role_chat, ChatRec).

get_role_pos_rec(RoleId) ->
	hd(gen_cache:lookup(?CACHE_REF(tab_role_pos), tab_role_pos, RoleId)).

get_role_chat_rec(RoleId) ->
	hd(gen_cache:lookup(?CACHE_REF(tab_role_chat), tab_role_chat, RoleId)).

update_role_pos_rec(RolePosRec) ->
	gen_cache:update(?CACHE_REF(tab_role_pos), tab_role_pos, RolePosRec).

get_role_level(RoleId) ->
	gen_cache:lookup_element(?CACHE_REF(tab_role_data), tab_role_data, 
							 RoleId, #tab_role_data.role_level).
