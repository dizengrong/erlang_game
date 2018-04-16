-module (mod_role).
-compile([export_all]).
-include ("common.hrl").
-include ("map.hrl").
-include ("log.hrl").
-include ("proto_login_pb.hrl").

handle(#mc_login{ret_code = Code, role_id = RoleId}, State) ->
	?DBG(Code),
	case Code of
		0 -> 
			State#robot{
				role_id   = RoleId,
				status    = ?ROBOT_STATUS_ENTER_GAME
			};
		1 ->
			State#robot{status = ?ROBOT_STATUS_REGISTER_ROLE}
	end;
handle(#mc_register{ret_code=Code, role_id = RoleId}, State) ->
	case Code of
		0 ->
			State#robot{
				role_id = RoleId,
				status  = ?ROBOT_STATUS_ENTER_GAME
			};
		_ ->
			erlang:throw({register_error, Code})
	end;
handle(#mc_enter_game{map_id = MapId, x = X, y = Y}, State) ->
	State#robot{
		map_id = MapId,
		pos    = #r_pos{x = X, y = Y},
		status = ?ROBOT_STATUS_ENTER_MAP
	};

handle(_Msg, State) ->
	% io:format("msg: ~p is unhandled\n", [erlang:element(1, Msg)]),
	State.
