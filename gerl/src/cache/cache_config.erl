-module (cache_config).
-compile([export_all]).

-include("gen_cache.hrl").
-include("db_tables.hrl").

%% 如果需要使用gen_cache的话，就想对应的record名填入
%% 返回值:[db_table_xxx]
%% db_table_xxx模块提供了tables/0方法来获取其中定义的数据库表
%% 同一个CacheName的gen_cache进程可以为多个sql_tables提供缓存服务
table_modules() ->
	[db_table_account,
	 db_table_achievement,
	 db_table_dragon,
	 db_table_item,
	 db_table_magic,
	 db_table_role,
	 db_table_rune
	].

cache_name(db_table_account) 	 -> cache_db_table_account;
cache_name(db_table_achievement) -> cache_db_table_achievement;
cache_name(db_table_dragon) 	 -> cache_db_table_dragon;
cache_name(db_table_item) 		 -> cache_db_table_item;
cache_name(db_table_magic) 		 -> cache_db_table_magic;
cache_name(db_table_role) 		 -> cache_db_table_role;
cache_name(db_table_rune) 		 -> cache_db_table_rune.

-spec is_multi_keys(Conf::#cache_config{}) -> boolean().
%% @doc 是否是多关键字表
is_multi_keys(Conf) -> (length(Conf#cache_config.key_fields) > 1).


conf(tab_account) ->
	#cache_config{
		index_field = #tab_account.role_id,
		key_fields  = [#tab_account.role_id]
	};
conf(tab_achievement) ->
	#cache_config{
		index_field = #tab_achievement.role_id,
		key_fields  = [#tab_achievement.role_id, #tab_achievement.achieve_id]
	};
conf(tab_dragon) ->
	#cache_config{
		index_field = #tab_dragon.role_id,
		key_fields  = [#tab_dragon.role_id, #tab_dragon.dragon_id]
	};
conf(tab_item) ->
	#cache_config{
		index_field = #tab_item.role_id,
		key_fields  = [#tab_item.role_id, #tab_item.item_id]
	};
conf(tab_magic_book) ->
	#cache_config{
		index_field = #tab_magic_book.role_id,
		key_fields  = [#tab_magic_book.role_id, #tab_magic_book.magic_id]
	};
conf(tab_magic_data) ->
	#cache_config{
		index_field = #tab_magic_data.role_id,
		key_fields  = [#tab_magic_data.role_id]
	};
conf(tab_role_pos) ->
	#cache_config{
		index_field = #tab_role_pos.role_id,
		key_fields  = [#tab_role_pos.role_id]
	};
conf(tab_role_data) ->
	#cache_config{
		index_field = #tab_role_data.role_id,
		key_fields  = [#tab_role_data.role_id]
	};
conf(tab_troops) ->
	#cache_config{
		index_field = #tab_troops.role_id,
		key_fields  = [#tab_troops.role_id, #tab_troops.place]
	};
conf(tab_fight_attr) ->
	#cache_config{
		index_field = #tab_fight_attr.role_id,
		key_fields  = [#tab_fight_attr.role_id]
	};
conf(tab_rune) ->
	#cache_config{
		index_field = #tab_rune.role_id,
		key_fields  = [#tab_rune.role_id]
	};
conf(tab_role_chat) ->
	#cache_config{
		index_field = #tab_role_chat.role_id,
		key_fields  = [#tab_role_chat.role_id]
	};
conf(Rec) ->
	erlang:throw({?MODULE, error_confg, 'conf/1', Rec}).
