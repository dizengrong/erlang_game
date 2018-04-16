%% @author dzR <dizengrong@gmail.com>
%% @doc map each table to the right db_table_xxx
%% This file is generated automaticly 
-module (db_module).
-compile([export_all]).

tables() -> [tab_account,tab_achievement,tab_dragon,tab_item,tab_magic_book,tab_magic_data,tab_role_chat,tab_role_pos,tab_role_data,tab_troops,tab_fight_attr,tab_rune,tab_log_item,tab_log_gold].

tab2db_mod(tab_account) -> db_table_account;
tab2db_mod(tab_achievement) -> db_table_achievement;
tab2db_mod(tab_dragon) -> db_table_dragon;
tab2db_mod(tab_item) -> db_table_item;
tab2db_mod(tab_magic_book) -> db_table_magic;
tab2db_mod(tab_magic_data) -> db_table_magic;
tab2db_mod(tab_role_chat) -> db_table_role;
tab2db_mod(tab_role_pos) -> db_table_role;
tab2db_mod(tab_role_data) -> db_table_role;
tab2db_mod(tab_troops) -> db_table_role;
tab2db_mod(tab_fight_attr) -> db_table_role;
tab2db_mod(tab_rune) -> db_table_rune;
tab2db_mod(tab_log_item) -> db_table_user_log;
tab2db_mod(tab_log_gold) -> db_table_user_log;

tab2db_mod(Tab) -> erlang:throw({?MODULE, error_confg, 'tab2db_mod/1', Tab}).
