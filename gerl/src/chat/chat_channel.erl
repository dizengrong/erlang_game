%% @author dzR <dizengrong@gmail.com>
%% @doc 聊天频道

-module (chat_channel).
-include ("chat.hrl").
-include ("log.hrl").
-include ("common.hrl").
-include ("proto_chat_pb.hrl").

-export([start_link/1, channel_server/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record (state, {
	channel 	%% 该进程的频道名
}).

channel_server(Channel) -> {global, Channel}.

start_link(Channel) ->
	gen_server:start_link(channel_server(Channel), ?MODULE, {Channel}, []).

%% @private
init({Channel}) ->
	chat_dict:init(Channel),
	{ok, #state{channel = Channel}}.

%% @private
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

%% @private
handle_cast(Request, State) ->
	try
		do_handle_request(Request, State)
	catch
		Type:Reason -> ?PRINT_STACKTRACE(Request, Type, Reason)
	end,
	{noreply, State}.

%% @private
handle_info(_Info, State) ->
	{noreply, State}.
%% @private
terminate(_Reason, _State) ->
	ok.
%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

do_handle_request({join, ChatEntityRec, Room}, State) ->
	join2(State, ChatEntityRec, Room),
	{noreply, State};
do_handle_request({leave, EntityId, Room}, State) ->
	leave2(State, EntityId, Room),
	{noreply, State};
do_handle_request({send_channel_msg, EntityId, Room, Content}, State) ->
	send_channel_msg2(State, EntityId, Room, Content),
	{noreply, State};
do_handle_request({send_p2p_msg, SenderId, ReceiverName, Content}, State) ->
	send_p2p_msg2(State, SenderId, ReceiverName, Content),
	{noreply, State}.	

join2(State, ChatEntityRec, Room) ->
	Channel = State#state.channel,
	case Channel of
		?CHANNEL_PRIVATE -> 
			chat_dict:set_chat_entity_rec(Channel, ChatEntityRec);
		?CHANNEL_WORLD ->
			chat_dict:set_chat_entity_rec(Channel, ChatEntityRec),
			chat_dict:add_world_chat_member(Channel, ChatEntityRec#r_chat_entity.id);
		_ ->
			{ok, ChatRoomRec} = ensure_room_created(Channel, Room),
			ChatRoomRec2      = add_member(ChatRoomRec, ChatEntityRec),
			chat_dict:set_chat_room_rec(Channel, ChatRoomRec2)
	end,
	ok.

add_member(ChatRoomRec, ChatEntityRec) ->
	Members = lists:keystore(ChatEntityRec#r_chat_entity.id, #r_chat_entity.id, 
							 ChatRoomRec#r_chat_room.members, ChatEntityRec),
	ChatRoomRec#r_chat_room{members = Members}.

remove_member(ChatRoomRec, EntityId) ->
	Members = lists:keydelete(EntityId, #r_chat_entity.id, 
							  ChatRoomRec#r_chat_room.members),
	ChatRoomRec#r_chat_room{members = Members}.

ensure_room_created(Channel, Room) ->
	case chat_dict:get_chat_room_rec(Channel, {Channel, Room}) of
		undefined   -> create_room(Channel, Room);
		ChatRoomRec -> {ok, ChatRoomRec}
	end.


create_room(Channel, Room) ->
	ChatRoomRec = #r_chat_room{
		room_key = {Channel, Room},
		members  = []
	},
	chat_dict:set_chat_room_rec(Channel, ChatRoomRec),
	{ok, ChatRoomRec}.

leave2(State, EntityId, Room) ->
	Channel = State#state.channel,
	case Channel of
		?CHANNEL_PRIVATE -> 
			chat_dict:delete_chat_entity_rec(Channel, EntityId);
		?CHANNEL_WORLD ->
			chat_dict:delete_chat_entity_rec(Channel, EntityId),
			chat_dict:delete_world_chat_member(Channel, EntityId);
		_ ->
			case chat_dict:get_chat_room_rec(Channel, {Channel, Room}) of
				undefined   -> ingore;
				ChatRoomRec -> 
					ChatRoomRec2 = remove_member(ChatRoomRec, EntityId),
					case length(ChatRoomRec2#r_chat_room.members) of
						0 -> chat_dict:delete_chat_room_rec(Channel, {Channel, Room});
						_ -> chat_dict:set_chat_room_rec(Channel, ChatRoomRec2)
					end
			end
	end.

send_channel_msg2(State, SenderId, Room, Content) ->
	Channel = State#state.channel,
	case chat_dict:get_chat_room_rec(Channel, {Channel, Room}) of
		undefined   -> 
			?ERROR("~p not exist when entity ~p send msg: ~p", 
							[{Channel, Room}, SenderId, Content]);
		ChatRoomRec -> 
			Members = ChatRoomRec#r_chat_room.members,
			SenderEntityRec = lists:keyfind(SenderId, #r_chat_entity.id, Members),
			case check_send_msg(SenderEntityRec) of
				true -> 
					send_to_members(Channel, SenderEntityRec, Members, Content);
				{error, RetCode} ->
					Msg = #mc_chat{ret_code = RetCode},
					gateway_dispatch:send_to_role(SenderId, Msg)
			end,
			ok
	end.

send_to_members(Channel, SenderEntityRec, Members, Content) ->
	_ = [send_to_receiver(Channel, SenderEntityRec, Rec#r_chat_entity.id, Content)
		 || Rec <- Members],
	ok.

send_to_receiver(Channel, SenderEntityRec, ReceiverId, Content) ->
	Msg = #mc_chat{
		ret_code = ?CHAT_CODE_OK,
		channel  = chat_api:channel_name_2_id(Channel),
		content  = Content,
		sender   = SenderEntityRec
	},
	gateway_dispatch:send_to_role(ReceiverId, Msg).

send_p2p_msg2(State, SenderId, ReceiverName, Content) ->
	Channel = State#state.channel,
	case chat_dict:get_chat_entity_rec(Channel, SenderId) of
		undefined ->
			?ERROR("Sender ~p has no entity data", [SenderId]);
		SenderEntityRec ->
			case check_send_p2p_msg(SenderEntityRec, ReceiverName) of
				{error, RetCode} ->
					Msg = #mc_chat{ret_code = RetCode},
					gateway_dispatch:send_to_role(SenderId, Msg);
				{true, ReceiverId} ->
					send_to_receiver(Channel, SenderEntityRec, SenderId, Content),
					send_to_receiver(Channel, SenderEntityRec, ReceiverId, Content)
			end
	end.

check_send_p2p_msg(SenderEntityRec, ReceiverName) ->
	case account_srv:get_role_id_by_name(ReceiverName) of
		0 -> {error, ?CHAT_CODE_NO_PLAYER};
		ReceiverId ->
			case online_srv:is_online(ReceiverId) of
				false -> {error, ?CHAT_CODE_OFFLINE};
				true  -> 
					case check_send_msg(SenderEntityRec) of
						true -> {true, ReceiverId};
						{error, RetCode} -> {error, RetCode}
					end
			end
	end.

check_send_msg(SenderEntityRec) ->
	case SenderEntityRec#r_chat_entity.no_speak of
		0  -> true;
		-1 -> {error, ?CHAT_CODE_NO_SPEAK};
		Time -> 
			IsNoSpeak = (Time > util_time:now_seconds()),
			?_IF(IsNoSpeak, {error, ?CHAT_CODE_NO_SPEAK}, true)
	end.

