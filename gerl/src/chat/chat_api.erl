%% @author dzR <dizengrong@gmail.com>
%% @doc 聊天系统对外的API接口

-module (chat_api).
-include ("chat.hrl").
-include ("spec_type.hrl").
-include ("db_table_role.hrl").
-include ("proto_chat_pb.hrl").

-export([chat/2]).
-export([join_p2p/1, leave_p2p/1, send_p2p_msg/3]).
-export([join_team/2, leave_team/2, send_team_msg/3]).
-export([join_world/1, leave_world/1, send_world_msg/2]).
-export([channel_name_2_id/1]).

-define(SERVER(Channel), chat_channel:channel_server(Channel)).

chat(SenderId, Msg) ->
	Content = Msg#ms_chat.content,
	% Channel = channel_id_2_name(Msg#ms_chat.channel),
	case Msg#ms_chat.channel of
		?CHANNEL_ID_PRIVATE ->
			send_p2p_msg(SenderId, Msg#ms_chat.receiver, Content);
		?CHANNEL_ID_TEAM ->
			TeamId = 1, %% todo:获取组队id
			send_team_msg(SenderId, TeamId, Content);
		?CHANNEL_ID_WORLD ->
			send_world_msg(SenderId, Content)
	end.

%% @doc 频道名称到频道id的映射
channel_name_2_id(?CHANNEL_PRIVATE) -> ?CHANNEL_ID_PRIVATE;
channel_name_2_id(?CHANNEL_TEAM) 	-> ?CHANNEL_ID_TEAM;
channel_name_2_id(?CHANNEL_WORLD) 	-> ?CHANNEL_ID_WORLD.

% %% @doc 频道id到频道名称的映射
% channel_id_2_name(?CHANNEL_ID_PRIVATE) -> ?CHANNEL_PRIVATE;
% channel_id_2_name(?CHANNEL_ID_TEAM) 	-> ?CHANNEL_TEAM;
% channel_id_2_name(?CHANNEL_ID_WORLD) 	-> ?CHANNEL_WORLD.

%% ============================== 私聊API ==============================
-spec join_p2p(EntityId::role_id()) -> ok.
%% @doc 加入私聊
join_p2p(EntityId) ->
	join(EntityId, ?CHANNEL_PRIVATE, EntityId).

-spec leave_p2p(EntityId::integer()) -> ok.
%% @doc 离开私聊
leave_p2p(EntityId) ->
	leave(EntityId, ?CHANNEL_PRIVATE, EntityId).

%% @doc 发送私聊消息
send_p2p_msg(SenderId, ReceiverName, Content) ->
	Server = ?SERVER(?CHANNEL_PRIVATE),
	gen_server:cast(Server, {send_p2p_msg, SenderId, ReceiverName, Content}).
%% ============================== 私聊API ==============================

%% ============================== 世界聊天API ==============================
join_world(EntityId) ->
	join(EntityId, ?CHANNEL_WORLD, world).
leave_world(EntityId) ->
	leave(EntityId, ?CHANNEL_WORLD, world).
send_world_msg(SenderId, Content) ->
	send_channel_msg(SenderId, ?CHANNEL_WORLD, world, Content).
%% ============================== 世界聊天API ==============================


%% ============================== 队伍聊天API ==============================
join_team(EntityId, TeamId) ->
	join(EntityId, ?CHANNEL_TEAM, TeamId).
leave_team(EntityId, TeamId) ->
	leave(EntityId, ?CHANNEL_TEAM, TeamId).
send_team_msg(SenderId, TeamId, Content) ->
	send_channel_msg(SenderId, ?CHANNEL_TEAM, TeamId, Content).
%% ============================== 队伍聊天API ==============================


%% ============================ 内部聊天调用接口 ===========================
-spec join(EntityId::role_id(), Channel::atom(), Room::term()) -> ok.
%% @doc 加入频道Channel中的聊天室Room
%% Room可以是任何erlang格式的数据，不一定非得是整型id
join(EntityId, Channel, Room) ->
	ChatEntityRec = make_chat_entity_rec(EntityId),
	gen_server:cast(?SERVER(Channel), {join, ChatEntityRec, Room}).

-spec leave(EntityId::integer(), Channel::atom(), Room::term()) -> ok.
%% @doc 离开频道Channel中的聊天室Room
%% EntityId聊天实体的id，比如玩家id
leave(EntityId, Channel, Room) ->
	gen_server:cast(?SERVER(Channel), {leave, EntityId, Room}).

-spec send_channel_msg(EntityId::integer(), Channel::atom(), 
			   		   Room::term(), Content::tuple()) -> ok.
%% @doc 通过频道Channel在聊天室Room内发送聊天消息，Content为聊天内容
send_channel_msg(EntityId, Channel, Room, Content) ->
	gen_server:cast(?SERVER(Channel), {send_channel_msg, EntityId, Room, Content}).

%% @doc 从缓存获取构造一个r_chat_entity record
make_chat_entity_rec(RoleId) ->
	ChatRec = db_role:get_role_chat_rec(RoleId),
	#r_chat_entity{
		id       = RoleId, 
		name     = db_account:get_role_name(RoleId),
		level    = db_role:get_role_level(RoleId),
		no_speak = ChatRec#tab_role_chat.no_speak
	}.

