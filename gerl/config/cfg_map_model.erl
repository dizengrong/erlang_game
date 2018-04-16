%% @doc 地图模型配置，每一个地图id对应这一个地图model

-module (cfg_map_model).
-compile([export_all]).

%% 地图model：
%% 		map_model_city:通用的城镇地图model

-spec get_model(MapId::integer()) -> atom().
get_model(10000) -> map_model_city;
get_model(10001) -> map_model_city;

get_model(MapId) -> erlang:throw({?MODULE, error_confg, 'get_model/1', MapId}).
