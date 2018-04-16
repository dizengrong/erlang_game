%% @author dzR <dizengrong@gmail.com>
%% @doc npc的字典数据模块

-module (npc_dict).
-include("map.hrl").
-include("npc.hrl").

-export([init/1, next_npc_id/0]).
-export([set_npc_rec/1, get_npc_rec/1]).
-export([get_all_npc_id_list/0, set_all_npc_id_list/1]).

init(_MapState) ->
	erlang:put(current_max_npc_uid, 1),
	set_all_npc_id_list([]),
	ok.

%% @doc 获取当前一个可用的最大的npc id，并递增current_max_npc_uid
next_npc_id() ->
	NpcId = erlang:get(current_max_npc_uid),
	erlang:put(current_max_npc_uid, 1 + NpcId),
	NpcId.

%% @doc 操作#r_npc{}的接口
get_npc_rec(NpcId)  -> erlang:get({npc, NpcId}).
set_npc_rec(NpcRec) -> erlang:put({npc, NpcRec#r_npc.id}, NpcRec), ok.

%% @doc 操作所有npc_id列表的接口
get_all_npc_id_list() 		   -> erlang:get(npc_id_list).
set_all_npc_id_list(NpcIdList) -> erlang:put(npc_id_list, NpcIdList), ok.

