%% This module automatically generated - do not edit
%% This module provides utilities for mapping field index to field name
%% get_field_name/2 get the field atom name by record and it's field index number
%% no_of_fields/1 return how many fields in the record

-module(record_fields).

-export([get_field_name/2,no_of_fields/1]).

no_of_fields(tab_account) -> 5;
no_of_fields(tab_achievement) -> 5;
no_of_fields(tab_dragon) -> 7;
no_of_fields(tab_item) -> 11;
no_of_fields(tab_magic_book) -> 6;
no_of_fields(tab_magic_data) -> 4;
no_of_fields(tab_role_chat) -> 3;
no_of_fields(tab_role_pos) -> 6;
no_of_fields(tab_role_data) -> 6;
no_of_fields(tab_troops) -> 7;
no_of_fields(tab_fight_attr) -> 12;
no_of_fields(tab_rune) -> 8;
no_of_fields(Other) -> exit({error,"Invalid Record Name: "++Other}).


get_field_name(tab_account, 2)-> id;
get_field_name(tab_account, 3)-> role_id;
get_field_name(tab_account, 4)-> account;
get_field_name(tab_account, 5)-> role_name;
get_field_name(tab_account, 6)-> last_login_time;

get_field_name(tab_achievement, 2)-> id;
get_field_name(tab_achievement, 3)-> role_id;
get_field_name(tab_achievement, 4)-> achieve_id;
get_field_name(tab_achievement, 5)-> achieve_level;
get_field_name(tab_achievement, 6)-> data;

get_field_name(tab_dragon, 2)-> id;
get_field_name(tab_dragon, 3)-> role_id;
get_field_name(tab_dragon, 4)-> dragon_id;
get_field_name(tab_dragon, 5)-> level;
get_field_name(tab_dragon, 6)-> exp;
get_field_name(tab_dragon, 7)-> magic;
get_field_name(tab_dragon, 8)-> rage;

get_field_name(tab_item, 2)-> id;
get_field_name(tab_item, 3)-> role_id;
get_field_name(tab_item, 4)-> item_id;
get_field_name(tab_item, 5)-> type_id;
get_field_name(tab_item, 6)-> location;
get_field_name(tab_item, 7)-> amount;
get_field_name(tab_item, 8)-> start_time;
get_field_name(tab_item, 9)-> end_time;
get_field_name(tab_item, 10)-> morale;
get_field_name(tab_item, 11)-> used_times;
get_field_name(tab_item, 12)-> attr;

get_field_name(tab_magic_book, 2)-> id;
get_field_name(tab_magic_book, 3)-> role_id;
get_field_name(tab_magic_book, 4)-> magic_id;
get_field_name(tab_magic_book, 5)-> book_id;
get_field_name(tab_magic_book, 6)-> magic_level;
get_field_name(tab_magic_book, 7)-> has_learned;

get_field_name(tab_magic_data, 2)-> id;
get_field_name(tab_magic_data, 3)-> role_id;
get_field_name(tab_magic_data, 4)-> max_scroll;
get_field_name(tab_magic_data, 5)-> magic_crystal;

get_field_name(tab_role_chat, 2)-> id;
get_field_name(tab_role_chat, 3)-> role_id;
get_field_name(tab_role_chat, 4)-> no_speak;

get_field_name(tab_role_pos, 2)-> id;
get_field_name(tab_role_pos, 3)-> role_id;
get_field_name(tab_role_pos, 4)-> cur_mapid;
get_field_name(tab_role_pos, 5)-> cur_x;
get_field_name(tab_role_pos, 6)-> cur_y;
get_field_name(tab_role_pos, 7)-> dir;

get_field_name(tab_role_data, 2)-> id;
get_field_name(tab_role_data, 3)-> role_id;
get_field_name(tab_role_data, 4)-> role_level;
get_field_name(tab_role_data, 5)-> role_exp;
get_field_name(tab_role_data, 6)-> reserve_troops;
get_field_name(tab_role_data, 7)-> bag_capacity;

get_field_name(tab_troops, 2)-> id;
get_field_name(tab_troops, 3)-> role_id;
get_field_name(tab_troops, 4)-> troops_id;
get_field_name(tab_troops, 5)-> troops_amount;
get_field_name(tab_troops, 6)-> place;
get_field_name(tab_troops, 7)-> level;
get_field_name(tab_troops, 8)-> exp;

get_field_name(tab_fight_attr, 2)-> id;
get_field_name(tab_fight_attr, 3)-> role_id;
get_field_name(tab_fight_attr, 4)-> hero_id;
get_field_name(tab_fight_attr, 5)-> leadership;
get_field_name(tab_fight_attr, 6)-> attack;
get_field_name(tab_fight_attr, 7)-> defence;
get_field_name(tab_fight_attr, 8)-> intel;
get_field_name(tab_fight_attr, 9)-> magic;
get_field_name(tab_fight_attr, 10)-> max_magic;
get_field_name(tab_fight_attr, 11)-> rage;
get_field_name(tab_fight_attr, 12)-> max_rage;
get_field_name(tab_fight_attr, 13)-> move_speed;

get_field_name(tab_rune, 2)-> id;
get_field_name(tab_rune, 3)-> role_id;
get_field_name(tab_rune, 4)-> str_rune;
get_field_name(tab_rune, 5)-> will_rune;
get_field_name(tab_rune, 6)-> magic_rune;
get_field_name(tab_rune, 7)-> str_rune_skills;
get_field_name(tab_rune, 8)-> will_rune_skills;
get_field_name(tab_rune, 9)-> magic_rune_skills;

get_field_name(Record,Field) -> exit({error,"Invalid Record Name and Field: " ++ {Record, Field}}).
