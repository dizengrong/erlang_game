%% @doc 物品配置

-module (cfg_item).
-include ("item.hrl").
-compile([export_all]).


get(1000000) -> #c_item{
	type_id     = 1000000,
	name        = "传送符",
	colour      = ?ITEM_COLOUR_WHITE,
	is_overlap  = true,
	location    = 0,
	endurance   = ?ENDURANCE_NEVER,
	requirement = undefined,
	lifetime    = ?LIFETIME_ENDLESS,
	is_bind     = true,
	firm_lv     = 0,
	attr        = [],
	use_effect  = undefined
};

get(ItemTypeId) -> erlang:throw({?MODULE, error_confg, 'get/1', ItemTypeId}).
