%% @doc 地图配置

-module (cfg_map).
-include ("map.hrl").
-compile([export_all]).

%% @doc 所有的地图id
all_maps() -> [10000, 10001].

get(10000) -> #c_map{
	id           = 10000,
	type         = ?MAP_TYPE_CITY,
	row          = 50,
	column       = 80,
	max_enter_lv = 0,
	min_enter_lv = 10,
	max_times    = 0
};
get(10001) -> #c_map{
	id           = 10001,
	type         = ?MAP_TYPE_CITY,
	row          = 50,
	column       = 80,
	max_enter_lv = 0,
	min_enter_lv = 10,
	max_times    = 0
};
get(MapId) -> erlang:throw({?MODULE, error_confg, 'get/1', MapId}).

%% @doc 地图的出生点
born_pos(10000) -> {1, 1};
born_pos(MapId) -> erlang:throw({?MODULE, error_confg, 'born_pos/1', MapId}).

-spec jump_points(SrcMap::integer(), 
				  DestMap::integer()) -> {X::integer(), Y::integer()} | []. 
%% @doc 从源地图跳转到目的地图的跳转点
jump_points(10000, 10001) -> {20, 20};
jump_points(_, _) -> [].

get_module(10000) -> cfg_mask_10000;
get_module(10001) -> cfg_mask_10001;
get_module(MapId) -> erlang:throw({?MODULE, error_confg, 'get_module/1', MapId}).

%% 是否可以移动
is_can_move(MapId, X, Y) ->
	(get_module(MapId)):is_can_move(X, Y).

