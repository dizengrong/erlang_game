
-record (tab_magic_data, {
	id = 0,
	role_id = null,
	max_scroll = 10,
	magic_crystal = 0}).

-record (tab_magic_book, {
	id = 0,
	role_id = null,
	magic_id = 0,
	book_id = 0,
	magic_level = 0,
	has_learned = false}).

