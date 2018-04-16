%% @author dzR <dizengrong@gmail.com>
%% @doc npc有限状态机模块，处理npc的各种状态逻辑

-module (npc_fsm).
-include("map.hrl").
-include("npc.hrl").
-include("log.hrl").
-include("common.hrl").

-export ([main_loop/1]).

main_loop(Tick) ->
	_ = [fsm(NpcId, Tick) || NpcId <- npc_dict:get_all_npc_id_list()],
	ok.

fsm(NpcId, Tick) ->
	try
		NpcRec = npc_dict:get_npc_rec(NpcId),
		?_IF(is_ready(NpcRec, Tick), do_fsm(NpcRec))
	catch
		Type:Reason -> ?PRINT_STACKTRACE(fsm, Type, Reason)
	end,
	ok.

%% @doc 判断是否准备好可以开始行动了
is_ready(NpcRec, Tick) ->
	(NpcRec#r_npc.next_tick =< Tick).

%% @doc 根据状态执行逻辑
do_fsm(NpcRec) ->
	{ok, NewNpcRec} = case NpcRec#r_npc.state of
		?NS_FIRST_BORN -> fsm_first_born(NpcRec);
		?NS_IDLE 	   -> fsm_idle(NpcRec)
	end,
	npc_dict:set_npc_rec(NewNpcRec).

%% @doc 第一次出生后执行的状态逻辑	
fsm_first_born(NpcRec) ->
	NewNpcRec = NpcRec#r_npc{state = ?NS_IDLE},
	{ok, NewNpcRec}.

%% @doc 空闲时执行的状态逻辑
fsm_idle(NpcRec) ->
	NpcId   = NpcRec#r_npc.id,
	CellRec = map_dict:get_object_cell_rec(?OBJECT_NPC, NpcId),
	MapId   = CellRec#r_cell_npc.pos#r_pos.map_id,
	Path    = path:random_path(MapId, CellRec#r_cell_npc.pos, 2),
	npc_walk(NpcRec, CellRec, Path).


%% @doc npc移动一条路径，Path为路径
npc_walk(_NpcRec, _CellRec, []) -> ok;
npc_walk(NpcRec, CellRec, Path) ->
	CellRec2 = CellRec#r_cell_npc{path = Path},
	%% 先移动
	map_api:broadcast_npc_move(CellRec2),
	%% 然后假设立马就移动到目的地了，就更新它的位置，并做九宫格广播
	%% todo:根据npc的移动速度做延迟check
	map_api:npc_move_check(CellRec, lists:last(Path)),
	TickUsed = util_map:move_used_tick(CellRec#r_cell_npc.move_speed, length(Path)),
	NewNpcRec = NpcRec#r_npc{next_tick = NpcRec#r_npc.next_tick + TickUsed},
	{ok, NewNpcRec}.
