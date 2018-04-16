-module (mod_map).
-compile([export_all]).
-include ("common.hrl").
-include ("log.hrl").
-include ("proto_map_pb.hrl").

handle(#mc_map_enter{}, State) ->
	% mod_monster:add_monsters(PMapMonsters),
	?DBG(State),
	State#robot{status = ?ROBOT_STATUS_IDLE};
	
handle(_Msg, State) ->
	% io:format("msg: ~p is unhandled\n", [erlang:element(1, Msg)]),
	State.
