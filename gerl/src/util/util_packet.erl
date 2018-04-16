%% @author dzR <dizengrong@gmail.com>
%% @doc 解包、打包协议

-module (util_packet).
-include ("log.hrl").

-export ([send/3, unpacket/1, get_msg_code/1]).
-export ([send_to_role/2]).

%% @doc 发送协议包
send(Transport, Socket, Msg) ->
	?INFO("send packet: ~p", [Msg]),
	Transport:send(Socket, packet(Msg)).

%% @doc 发送数据包给玩家
send_to_role(RoleId, Msg) ->
	gateway_dispatch:send_to_role(RoleId, Msg).

%% @doc 解包
unpacket(Binary) ->
	<<MsgCode:16, Bin/binary>> = Binary,
	Msg = erlang:binary_to_term(Bin),
	?INFO("packet: ~p", [Msg]),
	{MsgCode, Msg}.
	
%% @doc 打包
packet(Record) ->
	MsgCode = get_msg_code(element(1, Record)),
    <<MsgCode:16, (erlang:term_to_binary(Record))/binary>>.

get_msg_code(MsgRecordTag) ->
	protocol_code:code_num(MsgRecordTag).
