-module (pt).
-include ("log.hrl").
-compile([export_all]).

%% @doc 解包
unpacket(Binary) ->
	<<MsgCode:16, Bin/binary>> = Binary,
	Msg = erlang:binary_to_term(Bin),
	?ERROR("Recv packet: ~p", [Msg]),
	{MsgCode, Msg}.
	
%% @doc 打包
packet(Record) ->
	MsgCode = get_msg_code(element(1, Record)),
    <<MsgCode:16, (erlang:term_to_binary(Record))/binary>>.

get_msg_code(MsgRecordTag) ->
	protocol_code:code_num(MsgRecordTag).

