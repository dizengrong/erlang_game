%% @doc 生成协议
-module (gen_protocol).
-compile([export_all]).

-define (PROTO_DIR, "./proto/").
-define (INCLUDE_DIR, "./include/proto/").

start() ->
	{ok, Filenames} = file:list_dir(?PROTO_DIR),
	generate_protocal(Filenames),
	generate_code(Filenames).

generate_protocal([]) -> ok;
generate_protocal([Filename | Rest]) ->
	File = filename:join(?PROTO_DIR, Filename),
	case filelib:is_regular(File) andalso filename:extension(Filename) == ".proto" of
		true ->
			io:format("Filename:~p~n", [Filename]),
			% protobuffs_compile:generate_source(File, [{output_include_dir, ?INCLUDE_DIR}, {output_src_dir, "../../.."}]);
			protobuffs_compile:generate_hrl(File, [{output_include_dir, ?INCLUDE_DIR}]);
		false ->
			ignore
	end,
	generate_protocal(Rest).

generate_code(Filenames) ->
	File = "src/protocol_code.erl",
	{ok, Fd} = file:open(File, write),
	io:format(Fd, "~s", [protocol_code_head()]),
	CodeList = generate_code(Filenames, []),

	[io:format(Fd, "code_num(~p) -> ~p;\n", [RecTag, MsgCode]) || {RecTag, MsgCode} <- CodeList],
	io:format(Fd, "code_num(RecTag) -> {error_protocol_code, RecTag}.\n\n", []),

	[begin 
		case atom_to_list(RecTag) of
			[$m, $s | _] -> 
				io:format(Fd, "code_tag(c_2_s, ~p) -> ~p;\n", [MsgCode, RecTag]);
			_ ->
				io:format(Fd, "code_tag(s_2_c, ~p) -> ~p;\n", [MsgCode, RecTag])
		end
	 end || {RecTag, MsgCode} <- CodeList],
	io:format(Fd, "code_tag(Type, CodeNum) -> {error_protocol_tag, Type, CodeNum}.", []),

	file:close(Fd).

generate_code([], Acc) -> Acc;
	% io:format(Fd, "code_num(RecTag) -> {error_protocol_code, RecTag}.", []);
generate_code([Filename | Rest], Acc) ->
	File = filename:join(?PROTO_DIR, Filename),
	case filelib:is_regular(File) andalso filename:extension(Filename) == ".proto" of
		true  -> 
			{ok, ProtocolFd} = file:open(File, read),
			Acc2 = generate_code2(ProtocolFd, Acc);
		false -> 
			Acc2 = Acc
	end,
	generate_code(Rest, Acc2).
			

generate_code2(ProtocolFd, Acc) ->
	case file:read_line(ProtocolFd) of
		{ok, Line} ->
			case Line of
				[$/, $/, $$, $$, $b, $e, $g, $i, $n | _] ->
					generate_code3(ProtocolFd, Acc);
				_ ->
					generate_code2(ProtocolFd, Acc)
			end;
		eof -> %% 文件读到尾了
			io:format("Warning, no protocol code defined in this file\n\n"),
			Acc
	end.
generate_code3(ProtocolFd, Acc) ->
	{ok, Line} = file:read_line(ProtocolFd),
	case Line of
		[$/, $/, ${ | _] ->
			{RecTag, MsgCode} = util_str:str_to_term(string:strip(string:substr(Line, 3))),
			% io:format(Fd, "code_num(~p) -> ~p;\n", [RecTag, MsgCode]),
			Acc2 = [{RecTag, MsgCode} | Acc],
			generate_code3(ProtocolFd, Acc2);
		[$/, $/, $$, $$, $e, $n, $d | _] ->
			Acc;
		_ -> 
			generate_code3(ProtocolFd, Acc)
	end.


protocol_code_head() ->
"%% @author dzR <dizengrong@gmail.com>
%% @doc The relationship of the protocol code and the protocol tag
%% This file is automaticly generated

-module (protocol_code).
-compile([export_all]).

".
