%% @author dzR <dizengrong@gmail.com>
%% @doc The relationship of the protocol code and the protocol tag
%% This file is automaticly generated

-module (protocol_code).
-compile([export_all]).

code_num(ms_map_move_check) -> 11004;
code_num(mc_map_move) -> 11003;
code_num(ms_map_move) -> 11003;
code_num(mc_map_jump) -> 11002;
code_num(ms_map_jump) -> 11002;
code_num(mc_map_leave) -> 11001;
code_num(mc_map_enter) -> 11000;
code_num(ms_map_enter) -> 11000;
code_num(mc_enter_game) -> 10002;
code_num(ms_enter_game) -> 10002;
code_num(mc_register) -> 10001;
code_num(ms_register) -> 10001;
code_num(mc_login) -> 10000;
code_num(ms_login) -> 10000;
code_num(mc_chat) -> 12000;
code_num(ms_chat) -> 12000;
code_num(RecTag) -> {error_protocol_code, RecTag}.

code_tag(c_2_s, 11004) -> ms_map_move_check;
code_tag(s_2_c, 11003) -> mc_map_move;
code_tag(c_2_s, 11003) -> ms_map_move;
code_tag(s_2_c, 11002) -> mc_map_jump;
code_tag(c_2_s, 11002) -> ms_map_jump;
code_tag(s_2_c, 11001) -> mc_map_leave;
code_tag(s_2_c, 11000) -> mc_map_enter;
code_tag(c_2_s, 11000) -> ms_map_enter;
code_tag(s_2_c, 10002) -> mc_enter_game;
code_tag(c_2_s, 10002) -> ms_enter_game;
code_tag(s_2_c, 10001) -> mc_register;
code_tag(c_2_s, 10001) -> ms_register;
code_tag(s_2_c, 10000) -> mc_login;
code_tag(c_2_s, 10000) -> ms_login;
code_tag(s_2_c, 12000) -> mc_chat;
code_tag(c_2_s, 12000) -> ms_chat;
code_tag(Type, CodeNum) -> {error_protocol_tag, Type, CodeNum}.