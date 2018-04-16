
-record (tab_log_gold, {
	id = 0,
	role_id = null,
	old_gold = 0,
	new_gold = 0,
	in_or_use = 1,
	log_descript = []}).

-record (tab_log_item, {
	id = 0,
	role_id = null,
	item_typeid = 0,
	item_name = [],
	log_descript = []}).

