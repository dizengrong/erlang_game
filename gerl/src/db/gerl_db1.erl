%% @author dzR <dizengrong@gmail.com>
%% @doc We use amnesia([http://amnesia.sourceforge.net]) deal with mysql datas
%% This file is generated automaticly

-module (gerl_db1).
-compile([export_all]).
-include("db.hrl").


driver_info () ->
	db_conf:driver_info().

tables() -> [tab_account,tab_achievement,tab_dragon,tab_item,tab_magic_book,tab_magic_data,tab_role_chat,tab_role_pos,tab_role_data,tab_troops,tab_fight_attr,tab_rune,tab_log_item,tab_log_gold].

table(tab_account) -> 
	[{role_id,integer,[unique,not_null]},{account,varchar,[unique,not_null]},{role_name,varchar,not_null},{last_login_time,integer,[not_null,{default,0}]}];
table(tab_achievement) -> 
	[{role_id,integer,[index,not_null]},{achieve_id,integer,[index,not_null]},{achieve_level,integer,not_null},{data,term,[not_null,{default,[]}]}];
table(tab_dragon) -> 
	[{role_id,integer,[unique,not_null]},{dragon_id,integer,not_null},{level,integer,[not_null,{default,0}]},{exp,integer,[not_null,{default,0}]},{magic,integer,[not_null,{default,0}]},{rage,integer,[not_null,{default,0}]}];
table(tab_item) -> 
	[{role_id,integer,[index,not_null]},{item_id,integer,[index,not_null,{default,0}]},{type_id,integer,[not_null,{default,0}]},{location,integer,[not_null,{default,0}]},{embe_items,term,[not_null,{default,[]}]},{amount,integer,[not_null,{default,0}]},{start_time,integer,[not_null,{default,0}]},{end_time,integer,[not_null,{default,0}]},{morale,integer,[not_null,{default,0}]},{used_times,integer,[not_null,{default,0}]},{attr,{term,1024},[not_null,{default,[]}]},{is_bind,bool,[not_null,{default,true}]},{firm_exp,integer,[not_null,{default,0}]},{firm_lv,integer,[not_null,{default,0}]},{endurance,integer,[not_null,{default,0}]},{colour,integer,[not_null,{default,1}]}];
table(tab_magic_book) -> 
	[{role_id,integer,[index,not_null]},{magic_id,integer,[index,not_null,{default,0}]},{book_id,integer,[not_null,{default,0}]},{magic_level,integer,[not_null,{default,0}]},{has_learned,bool,[{default,false}]}];
table(tab_magic_data) -> 
	[{role_id,integer,[unique,not_null]},{max_scroll,integer,[not_null,{default,10}]},{magic_crystal,integer,[not_null,{default,0}]}];
table(tab_role_chat) -> 
	[{role_id,integer,[unique,not_null]},{no_speak,integer,[not_null,{default,0}]}];
table(tab_role_pos) -> 
	[{role_id,integer,[unique,not_null]},{cur_mapid,integer,not_null},{cur_x,integer,not_null},{cur_y,integer,not_null},{dir,integer,[not_null,{default,0}]}];
table(tab_role_data) -> 
	[{role_id,integer,[unique,not_null]},{role_level,integer,[not_null,{default,1}]},{role_exp,integer,[not_null,{default,0}]},{reserve_troops,integer,[not_null,{default,2}]},{bag_capacity,integer,[not_null,{default,50}]}];
table(tab_troops) -> 
	[{role_id,integer,[index,not_null]},{troops_id,integer,[not_null,{default,0}]},{troops_amount,integer,[not_null,{default,0}]},{place,integer,[index,not_null,{default,0}]},{level,integer,[not_null,{default,1}]},{exp,integer,[not_null,{default,0}]}];
table(tab_fight_attr) -> 
	[{role_id,integer,[unique,not_null]},{hero_id,integer,not_null},{leadership,integer,[not_null,{default,0}]},{attack,integer,[not_null,{default,0}]},{defence,integer,[not_null,{default,0}]},{intel,integer,[not_null,{default,0}]},{magic,integer,[not_null,{default,0}]},{max_magic,integer,[not_null,{default,0}]},{rage,integer,[not_null,{default,0}]},{max_rage,integer,[not_null,{default,0}]},{move_speed,integer,[not_null,{default,0}]}];
table(tab_rune) -> 
	[{role_id,integer,[unique,not_null]},{str_rune,integer,[not_null,{default,0}]},{will_rune,integer,[not_null,{default,0}]},{magic_rune,integer,[not_null,{default,0}]},{str_rune_skills,term,[not_null,{default,[]}]},{will_rune_skills,term,[not_null,{default,[]}]},{magic_rune_skills,term,[not_null,{default,[]}]}];
table(tab_log_item) -> 
	[{role_id,integer,[unique,not_null]},{item_typeid,integer,[not_null,{default,0}]},{item_name,varchar,[not_null,{default,[]}]},{log_descript,varchar,[not_null,{default,[]}]}];
table(tab_log_gold) -> 
	[{role_id,integer,[unique,not_null]},{old_gold,integer,[not_null,{default,0}]},{new_gold,integer,[not_null,{default,0}]},{in_or_use,integer,[not_null,{default,1}]},{log_descript,varchar,[not_null,{default,[]}]}];
table(Tab) -> erlang:throw({?MODULE, error_confg, 'table/1', Tab}).
