%% @author dzR <dizengrong@gmail.com>
%% @doc 从gen_cache中获取与魔法相关数据的接口，对应的表模块：db_table_magic

-module (db_magic).
-include ("db_table_magic.hrl").
-compile([export_all]).

-define(CACHE_REF(Tab), cache_util:get_cache_name(Tab)).

insert_magic_data_rec(MagicDataRec) ->
	gen_cache:insert(?CACHE_REF(tab_magic_data), tab_magic_data, MagicDataRec).
