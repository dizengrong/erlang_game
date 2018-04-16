%% @author dzR <dizengrong@gmail.com>
%% @doc 地图中的九宫格操作
%% 服务器按照行优先的方式，从左向右以1开始编号每个cell，如下所示：
%% 1 	2	3	4	5
%% 6	7	8	9 	10
%% 11	12	13	14	15
%% 这里一张地图被划分为3行5列，一共15个cell

-module (map_cell).
-include ("map.hrl").
-include ("log.hrl").

-export([cell_id/3, cell_id/1, get_9_cells_by_xy/3, get_9_cells_by_cell/2, 
		 get_leave_and_enter_cells/3]).

-define(CELL(X, Y, MapRows), Y div ?CELL_COLUMN * MapRows + X div ?CELL_ROW + 1).


-spec cell_id(MapId::integer(), X::integer(), Y::integer()) -> CellId::integer().
%% @doc 根据坐标点(X, Y)确定该点在哪个cell中
cell_id(MapId, X, Y) ->
	{Row, _Column} = map_dict:get_map_size(MapId),
	?CELL(X, Y, Row).

-spec cell_id(PosRec::#r_pos{}) -> CellId::integer().
cell_id(PosRec) ->
	MapId  = PosRec#r_pos.map_id,
	X      = PosRec#r_pos.x,
	Y      = PosRec#r_pos.y,
	cell_id(MapId, X, Y).

-spec get_9_cells_by_xy(MapId::integer(), X::integer(), Y::integer()) -> Cells::list().
%% @doc 根据坐标(X, Y)获取其所在地图周围的九宫格
get_9_cells_by_xy(MapId, X, Y) ->
	{Row, Column} = map_dict:get_map_size(MapId),
	CellId = ?CELL(X, Y, Row),
	get_9_cells_help(CellId, Column, Row).

-spec get_9_cells_by_cell(MapId::integer(), CellId::integer()) -> Cells::list().
%% @doc 根据地图的一个cellid获取其周围的九宫格
get_9_cells_by_cell(MapId, CellId) ->
	{Row, Column} = map_dict:get_map_size(MapId),
	get_9_cells_help(CellId, Column, Row).

get_9_cells_help(CellId, C, R) ->
	if
		CellId == 1 ->
			CellList = [CellId, CellId + 1, CellId + C, CellId + C + 1];
		CellId == C ->
			CellList = [CellId, CellId - 1, CellId + C, CellId + C - 1];
		CellId == (R - 1) * C + 1 ->
			CellList = [CellId, CellId - C, CellId - C + 1, CellId + 1];
		CellId == R * C -> 
			CellList = [CellId, CellId - C, CellId - C -  1, CellId - 1];
		CellId < C -> %% 在第一行
			CellList = [CellId, CellId - 1, CellId + 1, CellId + C, CellId - 1 + C, CellId + 1 + C];
		CellId > (R - 1) * C -> %% 最后一行
			CellList = [CellId, CellId - 1, CellId + 1, CellId - C, CellId - 1 - C, CellId + 1 - C];
		(CellId - 1) rem C == 0 -> %% 在第一类
			CellList = [CellId, CellId + 1, CellId - C, CellId + C, CellId + 1 + C, CellId + 1 - C];
		CellId rem C == 0 -> %% 在最后一列
			CellList = [CellId, CellId - 1, CellId - C, CellId + C, CellId - 1 + C, CellId - 1 - C];
		true -> 
			CellList = [CellId, CellId + 1, CellId + 1 - C, 
						CellId + 1 + C, CellId - 1, CellId - 1 - C, 
						CellId - 1 + C, CellId - C, CellId + C]
	end,
	CellList.

-spec get_leave_and_enter_cells(
		MapId::integer(), 
		OldCell::integer(), 
		NewCell::integer()) -> {LeaveCells::list(), EnterCells::list()} | [].
%% @doc 根据所在地图的2个相邻的新旧cell，获取进入和离开的cells
%% 注意如果OldCell和NewCell不是相邻的，则会抛出异常
get_leave_and_enter_cells(MapId, OldCell, NewCell) ->
	{Row, Column} = map_dict:get_map_size(MapId),
	get_leave_and_enter_cells(OldCell, NewCell, Row, Column).

get_leave_and_enter_cells(OldCell, NewCell, C, R) ->
	if
		OldCell == NewCell ->			%% the same cell
			[];
		OldCell + 1 == NewCell ->		%% move to right cell
			if
				(NewCell rem C) == 0 ->  %% 位于最右边
					if
						NewCell == C -> %% 位于最右边的右上角(NewCell位于最后一列)
							LeaveCells = [OldCell - 1, OldCell - 1 + C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						NewCell == C * R -> %% 位于最右边的右下角
							LeaveCells = [OldCell - 1, OldCell - 1 - C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell - 1, OldCell - 1 - C, OldCell - 1 + C],
							EnterCells = [],
							{LeaveCells, EnterCells}
					end;
				(OldCell-1) rem C == 0 -> %% OldCell位于第一列
					if
						OldCell == 1 -> %% 位于第一个格子
							LeaveCells = [],
							EnterCells = [NewCell + 1, NewCell + 1 + C],
							{LeaveCells, EnterCells};
						OldCell == (R- 1) * C + 1 -> %% 位于第一列的最后一个格子
							LeaveCells = [],
							EnterCells = [NewCell + 1, NewCell + 1 - C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [],
							EnterCells = [NewCell + 1, NewCell + 1 - C, NewCell + 1 + C],
							{LeaveCells, EnterCells}
					end;
				true ->
					if
						NewCell < C ->
							LeaveCells = [OldCell - 1, OldCell - 1 + C],
							EnterCells = [NewCell + 1, NewCell + 1 + C],
							{LeaveCells, EnterCells};
						NewCell > C * (R - 1) ->
							LeaveCells = [OldCell - 1, OldCell - 1 - C],
							EnterCells = [NewCell + 1, NewCell + 1 - C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell - 1, OldCell - 1 - C, OldCell - 1 + C],
							EnterCells = [NewCell + 1, NewCell + 1 + C, NewCell + 1 -C],
							{LeaveCells, EnterCells}
					end
			end;
		OldCell + 1 - C == NewCell ->	%% move ot right top cell
			if 
				NewCell < C ->
					if
						NewCell == 2 ->
							LeaveCells = [OldCell + C, OldCell + 1 + C],
							EnterCells = [NewCell + 1, NewCell + 1 + C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell - 1, OldCell - 1 - C, OldCell - 1 + C, OldCell + C, OldCell + 1 + C],
							EnterCells = [NewCell + 1, NewCell + 1 + C],
							{LeaveCells, EnterCells}
					end;
				(NewCell rem C) == 0 ->
					if
						NewCell == C ->
							LeaveCells = [OldCell - 1, OldCell - 1 - C, OldCell - 1 + C, OldCell + C, OldCell + 1 + C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						NewCell == C * (R - 1) ->
							LeaveCells = [OldCell - 1, OldCell - 1 - C],
							EnterCells = [NewCell - C, NewCell - 1 - C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell - 1, OldCell - 1 - C, OldCell - 1 + C, OldCell + C, OldCell + 1 + C],
							EnterCells = [NewCell - C, NewCell - 1 - C],
							{LeaveCells, EnterCells}
					end;
				true ->
					LeaveCells = [OldCell - 1, OldCell - 1 - C, OldCell - 1 + C, OldCell + C, OldCell + 1 + C],
					EnterCells = [NewCell + 1, NewCell + 1 + C, NewCell + 1 -C, NewCell - C, NewCell - 1 - C],
					{LeaveCells, EnterCells}
			end;
		OldCell + 1 + C == NewCell ->	%% move to right down cell 
			if
				OldCell < C ->
					if
						OldCell == 1 ->
							LeaveCells =  [],
							EnterCells = [NewCell + 1, NewCell + 1 + C, NewCell + 1 -C, NewCell + C, NewCell - 1 + C],
							{LeaveCells, EnterCells};
						OldCell + 1 == C ->
							LeaveCells = [OldCell - 1, OldCell - 1 + C],
							EnterCells = [NewCell + C, NewCell - 1 + C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell - 1, OldCell - 1 + C],
							EnterCells = [NewCell + 1, NewCell + 1 + C, NewCell + 1 -C, NewCell + C, NewCell - 1 + C],
							{LeaveCells, EnterCells}
					end;
				(OldCell - 1) rem C == 0 ->
					if
						OldCell == (R - 2) * C + 1 ->
							LeaveCells = [OldCell - C, OldCell + 1 - C],
							EnterCells = [NewCell + 1, NewCell + 1 -C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell - C, OldCell + 1 - C],
							EnterCells = [NewCell + 1, NewCell + 1 + C, NewCell + 1 -C, NewCell +C, NewCell - 1 + C],
							{LeaveCells, EnterCells}
					end;
				NewCell > (R - 1) * C ->
					if
						NewCell == R * C ->
							LeaveCells = [OldCell - 1, OldCell - 1 - C, OldCell - 1 + C, OldCell - C, OldCell + 1 - C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell - 1, OldCell - 1 - C, OldCell - 1 + C, OldCell - C, OldCell + 1 - C],
							EnterCells = [NewCell + 1, NewCell + 1 -C],
							{LeaveCells, EnterCells}
					end;
				(NewCell rem C) == 0 ->
					LeaveCells = [OldCell - 1, OldCell - 1 -C, OldCell - 1 + C, OldCell - C, OldCell + 1 - C],
					EnterCells = [NewCell + C, NewCell - 1 + C],
					{LeaveCells, EnterCells};
				true ->
					LeaveCells = [OldCell - 1, OldCell - 1 -C, OldCell - 1 + C, OldCell - C, OldCell + 1 - C],
					EnterCells = [NewCell + 1, NewCell + 1 + C, NewCell + 1 -C, NewCell +C, NewCell - 1 + C],
					{LeaveCells, EnterCells}
			end;
		OldCell == NewCell + 1 ->		%% move to left cell 
			if
				(OldCell rem C) == 0 ->
					if
						OldCell == C ->
							LeaveCells = [],
							EnterCells = [NewCell - 1, NewCell - 1 + C],
							{LeaveCells, EnterCells};
						OldCell == R * C ->
							LeaveCells = [],
							EnterCells = [NewCell - 1, NewCell - 1 - C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [],
							EnterCells = [NewCell - 1, NewCell - 1 - C, NewCell - 1 + C],
							{LeaveCells, EnterCells}
					end;
				((NewCell - 1) rem C) == 0 ->
					if
						NewCell == 1 ->
							LeaveCells = [OldCell + 1, OldCell + 1 +C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						NewCell == 1 + (R - 1) * C ->
							LeaveCells = [OldCell + 1, OldCell + 1 - C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell + 1, OldCell + 1 + C, OldCell + 1 - C],
							EnterCells = [],
							{LeaveCells, EnterCells}
					end;
				true ->
					if
						OldCell < C ->
							LeaveCells = [OldCell + 1, OldCell + 1 + C],
							EnterCells = [NewCell - 1, NewCell - 1 + C],
							{LeaveCells, EnterCells};
						OldCell > (R - 1)*C ->
							LeaveCells = [OldCell + 1, OldCell + 1 - C],
							EnterCells = [NewCell - 1, NewCell - 1 - C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell + 1, OldCell + 1 + C, OldCell + 1 - C],
							EnterCells = [NewCell - 1, NewCell - 1 - C, NewCell - 1 + C],
							{LeaveCells, EnterCells}
					end
			end;
		OldCell == NewCell + 1 + C ->	%% move to left top cell 
			if
				(OldCell rem C) == 0 ->
					if
						OldCell == R * C ->
							LeaveCells = [],
							EnterCells = [NewCell - 1, NewCell - 1 - C, NewCell - 1 + C, NewCell - C, NewCell - C + 1],
							{LeaveCells, EnterCells};
						OldCell == 2 * C ->
							LeaveCells = [OldCell + C, OldCell - 1 + C],
							EnterCells = [NewCell - 1, NewCell - 1 + C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell + C, OldCell - 1 + C],
							EnterCells = [NewCell - 1, NewCell - 1 - C, NewCell - 1 + C, NewCell - C, NewCell - C + 1],
							{LeaveCells, EnterCells}
					end;
				OldCell > (R - 1) * C ->
					if
						OldCell == (R - 1) * C + 2 ->
							LeaveCells = [OldCell + 1, OldCell + 1 - C],
							EnterCells = [NewCell - C, NewCell - C + 1],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell + 1, OldCell + 1 - C],
							EnterCells = [NewCell - 1, NewCell - 1 - C, NewCell - 1 + C, NewCell - C, NewCell - C + 1],
							{LeaveCells, EnterCells}
					end;
				NewCell < C ->
					if
						NewCell == 1 ->
							LeaveCells = [OldCell + 1, OldCell + 1 + C, OldCell + 1 - C, OldCell + C, OldCell - 1 + C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell + 1, OldCell + 1 + C, OldCell + 1 - C, OldCell + C, OldCell - 1 + C],
							EnterCells = [NewCell - 1, NewCell - 1 + C],
							{LeaveCells, EnterCells}
					end;
				((NewCell - 1) rem C) == 0 ->
					LeaveCells = [OldCell + 1, OldCell + 1 + C, OldCell + 1 - C, OldCell + C, OldCell - 1 + C],
					EnterCells = [NewCell - C, NewCell - C + 1],
					{LeaveCells, EnterCells};
				true ->
					LeaveCells = [OldCell + 1, OldCell + 1 + C, OldCell + 1 - C, OldCell + C, OldCell - 1 + C],
					EnterCells = [NewCell - 1, NewCell - 1 - C, NewCell - 1 + C, NewCell - C, NewCell - C + 1],
					{LeaveCells, EnterCells}
			end;
		OldCell == NewCell + 1 - C ->	%% move to left down cell 
			if
				OldCell < C ->
					if
						OldCell == 2 ->
							LeaveCells = [OldCell + 1, OldCell + 1 +C],
							EnterCells = [NewCell + C, NewCell + C + 1],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell + 1, OldCell + 1 + C],
							EnterCells = [NewCell - 1, NewCell - 1 - C, NewCell - 1 + C, NewCell +C, NewCell + C + 1],
							{LeaveCells, EnterCells}
					end;
				(OldCell rem C) == 0 ->
					if
						OldCell == C ->
							LeaveCells = [],
							EnterCells = [NewCell - 1, NewCell - 1 - C, NewCell - 1 + C, NewCell + C, NewCell + C + 1],
							{LeaveCells, EnterCells};
						OldCell == (R - 1) * C ->
							LeaveCells = [OldCell - C, OldCell - 1 - C],
							EnterCells = [NewCell - 1, NewCell - 1 - C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell - C, OldCell - 1 - C],
							EnterCells = [NewCell - 1, NewCell - 1 - C, NewCell - 1 + C, NewCell + C, NewCell + C + 1],
							{LeaveCells, EnterCells}
					end;
				((NewCell - 1) rem C == 0) ->
					if
						NewCell == C * (R - 1) + 1 ->
							LeaveCells = [OldCell + 1, OldCell + 1 + C, OldCell + 1 - C, OldCell - C, OldCell - 1 - C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell + 1, OldCell + 1 + C, OldCell + 1 - C, OldCell - C, OldCell - 1 - C],
							EnterCells = [NewCell + C, NewCell + C + 1],
							{LeaveCells, EnterCells}
					end;
				NewCell > C * (R - 1) ->
					LeaveCells = [OldCell + 1, OldCell + 1 + C, OldCell + 1 - C, OldCell - C, OldCell - 1 -C],
					EnterCells = [NewCell - 1, NewCell - 1 - C],
					{LeaveCells, EnterCells};
				true ->
					LeaveCells = [OldCell + 1, OldCell + 1 + C, OldCell + 1 - C, OldCell - C, OldCell - 1 -C],
					EnterCells = [NewCell - 1, NewCell - 1 - C, NewCell - 1 + C, NewCell + C, NewCell + C + 1],
					{LeaveCells, EnterCells}
			end;
		OldCell == NewCell + C->	%% move to up cell 
			if
				NewCell =< C ->
					if
						NewCell == 1 ->
							LeaveCells = [OldCell +C, OldCell + 1 + C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						NewCell == C ->
							LeaveCells = [OldCell + C, OldCell - 1 + C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell + C, OldCell + 1 + C, OldCell - 1 + C],
							EnterCells = [],
							{LeaveCells, EnterCells}
					end;
				OldCell > (R - 1) * C ->
					if
						OldCell == (R - 1)*C + 1 ->
							LeaveCells = [],
							EnterCells = [NewCell - C, NewCell + 1 - C],
							{LeaveCells, EnterCells};
						OldCell == R*C ->
							LeaveCells = [],
							EnterCells = [NewCell - C, NewCell - 1 - C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [],
							EnterCells = [NewCell - C, NewCell - 1 - C, NewCell + 1 - C],
							{LeaveCells, EnterCells}
					end;
				(OldCell - 1) rem C == 0 ->
					LeaveCells = [OldCell + C, OldCell + 1 + C],
					EnterCells = [NewCell - C, NewCell + 1 - C],
					{LeaveCells, EnterCells};
				OldCell rem C == 0 ->
					LeaveCells = [OldCell + C, OldCell - 1 + C],
					EnterCells = [NewCell - C, NewCell - 1 - C],
					{LeaveCells, EnterCells};
				true ->
					LeaveCells = [OldCell + C, OldCell + 1 + C, OldCell - 1 + C],
					EnterCells = [NewCell - C, NewCell - 1 - C, NewCell + 1 - C],
					{LeaveCells, EnterCells}
			end;
		OldCell == NewCell - C ->	%% move to bottom cell
			if
				NewCell > (R - 1) * C -> %% 新格子在最后一行
					if
						NewCell == (R - 1) * C + 1 ->
							LeaveCells = [OldCell - C, OldCell + 1 - C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						NewCell == R * C ->
							LeaveCells = [OldCell - C, OldCell - 1 - C],
							EnterCells = [],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell - C, OldCell + 1 - C, OldCell - 1 - C],
							EnterCells = [],
							{LeaveCells, EnterCells}
					end;
				OldCell =< C -> %% 旧格子在第一行
					if
						OldCell == 1 ->
							LeaveCells = [],
							EnterCells = [NewCell + C, NewCell + 1 + C],
							{LeaveCells, EnterCells};
						OldCell == C ->
							LeaveCells = [],
							EnterCells = [NewCell + C, NewCell - 1 + C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [],
							EnterCells = [NewCell + C, NewCell - 1 + C, NewCell + 1 + C],
							{LeaveCells, EnterCells}
					end;
				true -> %% 在中间
					if
						((NewCell - 1) rem C == 0) -> %% 在第一列
							LeaveCells = [OldCell - C, OldCell + 1 - C],
							EnterCells = [NewCell + C, NewCell + 1 + C],
							{LeaveCells, EnterCells};
						(NewCell rem C == 0) -> %% 在最后列
							LeaveCells = [OldCell - C, OldCell - 1 - C],
							EnterCells = [NewCell + C, NewCell - 1 + C],
							{LeaveCells, EnterCells};
						true ->
							LeaveCells = [OldCell - C, OldCell + 1 - C, OldCell - 1 - C],
							EnterCells = [NewCell + C, NewCell - 1 + C, NewCell + 1 + C],
							{LeaveCells, EnterCells}
					end
			end;
		true -> %% 异常bug！！！
			?ERROR("OldCell = ~w, NewCell = ~w", [OldCell, NewCell]),
			exit("get_leave_and_enter_cells heavey bug!!!")
	end.