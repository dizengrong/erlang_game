
-record (tab_account, {
	id = 0,
	role_id = null,
	account = null,
	role_name = null,
	last_login_time = 0}).

-record (tab_achievement, {
	id = 0,
	role_id = null,
	achieve_id = null,
	achieve_level = null,
	data = []}).

-record (tab_dragon, {
	id = 0,
	role_id = null,
	dragon_id = null,
	level = 0,
	exp = 0,
	magic = 0,
	rage = 0}).

-record (tab_item, {
	id = 0,
	role_id = null,
	item_id = 0,
	type_id = 0,
	location = 0,
	embe_items = [],
	amount = 0,
	start_time = 0,
	end_time = 0,
	morale = 0,
	used_times = 0,
	attr = [],
	is_bind = true,
	firm_exp = 0,
	firm_lv = 0,
	endurance = 0,
	colour = 1}).

-record (tab_magic_book, {
	id = 0,
	role_id = null,
	magic_id = 0,
	book_id = 0,
	magic_level = 0,
	has_learned = false}).

-record (tab_magic_data, {
	id = 0,
	role_id = null,
	max_scroll = 10,
	magic_crystal = 0}).

-record (tab_role_chat, {
	id = 0,
	role_id = null,
	no_speak = 0}).

-record (tab_role_pos, {
	id = 0,
	role_id = null,
	cur_mapid = null,
	cur_x = null,
	cur_y = null,
	dir = 0}).

-record (tab_role_data, {
	id = 0,
	role_id = null,
	role_level = 1,
	role_exp = 0,
	reserve_troops = 2,
	bag_capacity = 50}).

-record (tab_troops, {
	id = 0,
	role_id = null,
	troops_id = 0,
	troops_amount = 0,
	place = 0,
	level = 1,
	exp = 0}).

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

-record (tab_rune, {
	id = 0,
	role_id = null,
	str_rune = 0,
	will_rune = 0,
	magic_rune = 0,
	str_rune_skills = [],
	will_rune_skills = [],
	magic_rune_skills = []}).

-record (tab_log_item, {
	id = 0,
	role_id = null,
	item_typeid = 0,
	item_name = [],
	log_descript = []}).

-record (tab_log_gold, {
	id = 0,
	role_id = null,
	old_gold = 0,
	new_gold = 0,
	in_or_use = 1,
	log_descript = []}).

