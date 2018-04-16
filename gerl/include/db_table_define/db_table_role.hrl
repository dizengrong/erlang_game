
-record (tab_fight_attr, {
	id = 0,
	role_id = null,
	hero_id = null,
	leadership = 0,
	attack = 0,
	defence = 0,
	intel = 0,
	magic = 0,
	max_magic = 0,
	rage = 0,
	max_rage = 0,
	move_speed = 0}).

-record (tab_troops, {
	id = 0,
	role_id = null,
	troops_id = 0,
	troops_amount = 0,
	place = 0,
	level = 1,
	exp = 0}).

-record (tab_role_data, {
	id = 0,
	role_id = null,
	role_level = 1,
	role_exp = 0,
	reserve_troops = 2,
	bag_capacity = 50}).

-record (tab_role_pos, {
	id = 0,
	role_id = null,
	cur_mapid = null,
	cur_x = null,
	cur_y = null,
	dir = 0}).

-record (tab_role_chat, {
	id = 0,
	role_id = null,
	no_speak = 0}).

