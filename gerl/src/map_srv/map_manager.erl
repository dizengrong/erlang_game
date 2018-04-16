%% @author dzR <dizengrong@gmail.com>
%% @doc 地图管理器，管理地图的创建等

-module (map_manager).
-include ("map.hrl").

-export([pre_create_maps/0, create_map/1, create_team_map/2, create_single_map/2]).

%% @doc 预创建那些需要创建的地图
pre_create_maps() ->
	[pre_create_maps(MapId) || MapId <- cfg_map:all_maps()].
pre_create_maps(MapId) ->
	CMapRec = cfg_map:get(MapId),
	case CMapRec#c_map.type == ?MAP_TYPE_CITY andalso
		 gerl_setting:get({map_node, MapId}) == node() of
		true ->
			create_map(MapId);
		_ -> ignore
	end.

-spec create_map(MapId::integer()) -> {ok, Node::node(), RegName::atom()}.
%% @doc 创建一个地图进程，该地图类型是城镇或是多人地图
create_map(MapId) ->
	RegName   = map_register_name(MapId),
	create_map_help(MapId, RegName).

%% @doc 创建一个地图进程，该地图类型是组队
%% 返回值同create_map/1
create_team_map(MapId, TeamId) ->
	RegName   = map_register_name(MapId, TeamId),
	create_map_help(MapId, RegName).

%% @doc 创建一个地图进程，该地图类型是单人的
%% 返回值同create_map/1
create_single_map(MapId, RoleId) ->
	RegName   = map_register_name(MapId, RoleId),
	create_map_help(MapId, RegName).

create_map_help(MapId, RegName) ->
	StartFunc = {map_srv, start_link, [MapId, RegName]},
	Spec      = {RegName, StartFunc, permanent, 10000, worker, [map_srv]},
	Node      = node(),
	WhichNode = gerl_setting:get({map_node, MapId}),
	case WhichNode == Node of
		true  -> gerl_map_srv_sup:start_child(Spec);
		false -> rpc:cast(WhichNode, gerl_map_srv_sup, start_child, [Spec])
	end,
	map_dispatch:register_map(MapId, WhichNode, RegName),
	{ok, WhichNode, RegName}.

map_register_name(MapId) ->
	map_register_name(MapId, 0).
map_register_name(MapId, Arg) ->
	CMapRec = cfg_map:get(MapId),
	Name = case CMapRec#c_map.type of
		?MAP_TYPE_CITY   -> lists:concat(["map_city_", MapId]);
		?MAP_TYPE_SINGLE -> lists:concat(["map_single_", MapId, "_", Arg]);
		?MAP_TYPE_TEAM   -> lists:concat(["map_team_", MapId, "_", Arg]);
		?MAP_TYPE_MULTI  -> lists:concat(["map_multi_", MapId, "_", Arg])
	end,
	list_to_atom(Name).