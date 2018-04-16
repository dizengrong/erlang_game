%% @author dzR <dizengrong@gmail.com>
%% @doc 客户端协议请求处理：地图相关

-module (pp_map).
-include("log.hrl").
-include("map.hrl").
-include("proto_map_pb.hrl").

-export([client_reqeust/4]).

%% @doc 客户端请求处理
-spec client_reqeust(ProtocolCodeTag::atom(), 
					 RoleId::integer(),
					 Msg::tuple(),
					 State::#map_state{}) -> any().	
client_reqeust(ms_map_enter, RoleId, _Msg, _State) -> 
	map_event:role_enter(RoleId),
	CellRoleRec = map_api:make_new_cell_role(RoleId),
	map_api:add_role_to_cell(CellRoleRec),
	map_api:broadcast_role_enter(CellRoleRec),
	ok;

client_reqeust(ms_map_jump, RoleId, Msg, _State) ->
	DestMapId = Msg#ms_map_jump.dest_map_id,
	PosRec    = map_dict:get_pos(?OBJECT_ROLE, RoleId),
	SrcMapId  = PosRec#r_pos.map_id,
	X         = PosRec#r_pos.x,
	Y         = PosRec#r_pos.y,
	case cfg_map:jump_points(SrcMapId, DestMapId) of
		[] -> 
			?ERROR("There is no jump point from map ~p to map ~p", [SrcMapId, DestMapId]);
		{JumpX, JumpY} ->
			case util_pos:is_in_range(JumpX, JumpY, X, Y, 3) of
				true ->
					{DestX, DestY} = cfg_map:born_pos(DestMapId),
					map_api:role_jump_to(RoleId, DestMapId, DestX, DestY);
				false ->
					?WARNING("Not in range when jump")
			end
	end;
client_reqeust(ms_map_move, RoleId, Msg, _State) ->
	map_api:role_move_path(RoleId, Msg#ms_map_move.path),
	ok;
client_reqeust(ms_map_move_check, RoleId, Msg, _State) ->
	map_api:role_move_check(RoleId, Msg#ms_map_move_check.x, Msg#ms_map_move_check.y),
	ok.