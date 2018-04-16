%% @doc 地图配置

-module (cfg_map).
-compile([export_all]).

%% @doc 所有的地图id
all_maps() -> [10000, 10001].

get(MapId) -> (get_module(MapId)):get().

%% @doc 地图的出生点
born_pos(MapId) -> (get_module(MapId)):born_pos().

-spec jump_points(SrcMap::integer(), 
				  DestMap::integer()) -> {X::integer(), Y::integer()} | []. 
%% @doc 从源地图跳转到目的地图的跳转点
jump_points(10000, 10001) -> {20, 20};
jump_points(_, _) -> [].

get_module(10000) -> cfg_map_10000;
get_module(10001) -> cfg_map_10001;
get_module(MapId) -> erlang:throw({?MODULE, error_confg, 'get_module/1', MapId}).

%% 是否可以移动
is_can_move(MapId, X, Y) ->
	(get_module(MapId)):is_can_move(X, Y).

npc(MapId) ->
	(get_module(MapId)):npc().