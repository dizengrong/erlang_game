%% @author dzR <dizengrong@gmail.com>
%% @doc 聊天频道的字典数据

-module (chat_dict).
-include ("chat.hrl").

-export ([init/1]).
-export ([get_chat_room_rec/2, set_chat_room_rec/2, delete_chat_room_rec/2]).
-export ([get_chat_entity_rec/2, set_chat_entity_rec/2, delete_chat_entity_rec/2]).
-export ([get_world_chat_member/1, 
		  add_world_chat_member/2, delete_world_chat_member/2]).

init(_Channel) ->
	ok.

%% @doc 操作chat_room的接口
get_chat_room_rec(Channel, RoomKey) -> erlang:get({Channel, RoomKey}).
delete_chat_room_rec(Channel, RoomKey) -> erlang:erase({Channel, RoomKey}).
set_chat_room_rec(Channel, ChatRoomRec) -> 
	erlang:put({Channel, ChatRoomRec#r_chat_room.room_key}, ChatRoomRec).


%% @doc 私聊的实体操作接口
get_chat_entity_rec(Channel, EntityId) -> erlang:get({Channel, EntityId}).
set_chat_entity_rec(Channel, ChatEntityRec) ->
	erlang:put({Channel, ChatEntityRec#r_chat_entity.id}, ChatEntityRec).
delete_chat_entity_rec(Channel, EntityId) -> erlang:erase({Channel, EntityId}).


%% @doc 操作世界聊天成员的接口
get_world_chat_member(Channel) -> erlang:get({Channel, world_members}).
set_world_chat_member(Channel, Members) -> 
	erlang:put({Channel, world_members}, Members).
add_world_chat_member(Channel, EntityId) -> 
	Members = get_world_chat_member(Channel),
	case lists:member(EntityId, Members) of
		true  -> ok;
		false -> set_world_chat_member(Channel, [EntityId | Members])
	end,
	ok.
delete_world_chat_member(Channel, EntityId) ->
	Members = get_world_chat_member(Channel),
	set_world_chat_member(Channel, lists:delete(EntityId, Members)),
	ok.
