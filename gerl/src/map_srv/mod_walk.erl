-module(mod_walk).
-include ("map.hrl").
-export([
         get_walk_path/3
        ]).


%%----------------------------------------------------------------
%% 获得走路路径  先采用直线，不行再走高级寻路
%% @CurrentPos 当前位置#r_pos
%% @GotoPos 要前往的位置#r_pos
%% return Path: [{X, Y, Dir}]
%%----------------------------------------------------------------
get_walk_path(MapId, CurrentPos, GotoPos) ->
    case get_straight_line_path(MapId, CurrentPos, GotoPos, []) of
        false ->
            get_senior_path(MapId, CurrentPos, GotoPos);
        {ok,Path} ->
            {ok, Path}
    end.


%%----------------------------------------------------------------
%% 直线寻路
%% @CurrentPos 当前位置#r_pos
%% @GotoPos 要前往的位置#r_pos
%% @Path路径信息
%%----------------------------------------------------------------

get_straight_line_path(MapId, CurrentPos,GotoPos,Path) ->
    get_straight_line_path(MapId, CurrentPos,GotoPos,Path,{-10000,-10000}).


get_straight_line_path(MapId, CurrentPos,GotoPos,Path,{LastTx,LastTy}) ->
    #r_pos{x = Tx1, y = Ty1, dir=Dir1} = CurrentPos,
    #r_pos{x = Tx2, y = Ty2, dir = Dir2} = GotoPos,
    case abs(Tx1-Tx2)=<1 andalso abs(Ty1-Ty2)=<1 of
        true ->
            {ok,lists:reverse([{Tx2,Ty2,Dir2}|Path])};
        false ->
            PosList = get_straight_line_pos_list(Tx1, Ty1, Tx2, Ty2),
            case get_empty_grid(MapId, PosList) of
                false ->
                    false;
               #r_pos{x=Tx,y=Ty} = NextPos ->
                    case Tx =:= LastTx andalso Ty =:= LastTy of
                        true ->
                            false;
                        false ->
                           
                            get_straight_line_path(MapId, NextPos,GotoPos,[{Tx1,Ty1,Dir1}|Path],{Tx1,Ty1})
                    end
            end
    end.

%%----------------------------------------------------------------
%% 高级寻路，目前用的使用A*寻路
%% @CurrentPos 当前位置#r_pos
%% @GotoPos 要前往的位置#r_pos
%%----------------------------------------------------------------
get_senior_path(MapId, CurrentPos, GotoPos)->
   case mod_astar_pathfinding:find_path(MapId, CurrentPos, GotoPos) of
       false ->
           false;
       Path ->
           {ok,Path}
    end.

get_empty_grid(_MapID, []) ->
    false;

get_empty_grid(MapID, [{X,Y,Dir}|List]) ->
    case cfg_map:is_can_move(MapID, X, Y) of 
        false ->
            get_empty_grid(MapID, List);
        _ ->
            #r_pos{x = X, y = Y, dir = Dir}
    end.

   
get_straight_line_pos_list(Tx1, Ty1, Tx2, Ty2) ->
    case Tx1 < Tx2 of
        true ->
            case Ty1 < Ty2 of
                true ->
                    [{Tx1+1, Ty1+1, 4}, {Tx1+1, Ty1, 3}, {Tx1, Ty1+1, 5}];
                false ->
                    case Ty1 > Ty2 of
                        true ->
                            [{Tx1+1, Ty1-1, 2}, {Tx1+1, Ty1, 3}, {Tx1, Ty1-1, 1}];
                        false ->
                            [{Tx1+1, Ty1, 3}, {Tx1+1, Ty1-1, 2}, {Tx1+1, Ty1+1, 4}]
                    end
            end;
        false ->
            case Tx1 > Tx2 of
                true ->
                    case Ty1 < Ty2 of
                        true ->
                            [{Tx1-1, Ty1+1, 6}, {Tx1-1, Ty1, 7}, {Tx1, Ty1+1, 5}];
                        false ->
                            case Ty1 > Ty2 of
                                true ->
                                    [{Tx1-1, Ty1-1, 0}, {Tx1, Ty1-1, 1}, {Tx1-1, Ty1, 7}];
                                false ->
                                    [{Tx1-1, Ty1, 7}, {Tx1-1, Ty1-1, 0}, {Tx1-1, Ty1+1, 6}]
                            end
                    end;
                false ->
                    case Ty1 < Ty2 of
                        true ->
                            [{Tx1, Ty1+1, 5}, {Tx1+1, Ty1+1, 4}, {Tx1-1, Ty1+1, 6}];
                        false ->
                            [{Tx1, Ty1-1, 1}, {Tx1-1, Ty1-1, 0}, {Tx1+1, Ty1-1, 2}]
                    end
            end
    end.
