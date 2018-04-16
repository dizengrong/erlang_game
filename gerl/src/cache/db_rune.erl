%% @author dzR <dizengrong@gmail.com>
%% @doc 从gen_cache中获取与技能符石相关数据的接口，对应的表模块：db_table_rune

-module (db_rune).
-include ("db_table_rune.hrl").
-compile([export_all]).

-define(CACHE_REF(Tab), cache_util:get_cache_name(Tab)).

insert_rune_rec(RuneRec) ->
	gen_cache:insert(?CACHE_REF(tab_rune), tab_rune, RuneRec).
