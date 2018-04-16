%% @author dzR <dizengrong@gmail.com>
%% @doc 地图分发服务进程，用来管理玩家在哪个地图进程的数据
%% 当玩家所在的地图进程改变了，会首先通知这个进程
%% 从客户端来的网关消息要路由到对应的地图进程，因此会到这里来查询

-module (map_dispatch).
-include("log.hrl").
-include("map.hrl").

-export([start_link/0, register_map/3, register_team_map/4, query_map_name/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, {global, ?MODULE}).

start_link() ->
	init_table(),
	gen_server:start_link(?SERVER, ?MODULE, {}, []).

init_table() ->
    ets:new(ets_map_dispatch, [public,named_table,set,{keypos, 1}]).

% -spec register(RoleId::integer(), MapNode::node(), MapRegName::atom()) -> any().
% %% @doc 注册玩家当前所在地图的地图进程pid
% register(RoleId, MapNode, MapRegName) ->
% 	erlang:send(global:whereis_name(?MODULE), {register, RoleId, MapNode, MapRegName}).

-spec register_map(MapId::integer(), 
				   MapNode::node(), 
				   MapRegName::atom()) -> any().
%% @doc 注册一个非组队类型地图的名称
register_map(MapId, MapNode, MapRegName) ->
	erlang:send(global:whereis_name(?MODULE), {register_map, MapId, MapNode, MapRegName}).

-spec register_team_map(MapId::integer(), 
						TeamId::integer(), 
						MapNode::node(), 
						MapRegName::atom()) -> any().
%% @doc 注册一个组队类型地图的名称
register_team_map(MapId, TeamId, MapNode, MapRegName) ->
	erlang:send(global:whereis_name(?MODULE), {register_map, {MapId, TeamId}, MapNode, MapRegName}).

-spec query_map_name(MapId::integer() | 
		{MapId::integer(), TeamId::integer()}) -> {node(), atom()} | undefined.
%% @doc 查询玩家当前所在地图的进程名:{Node, RegName}
%% MapKey:为地图id，如果是非组队地图；否则为{地图id, 组队id}
query_map_name(MapKey) ->
	gen_server:call(?SERVER, {query_map_name, MapKey}).

%% @private
init({}) ->
    erlang:process_flag(trap_exit, true),
	{ok, undefined}.

%% @private
handle_call({query_map_name, MapKey}, _From, State) ->
	Reply = case ets:lookup(ets_map_dispatch, MapKey) of
		[] -> undefined;
		[{_, Name}] -> Name
	end,
	{reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
%% 参数MapKey: MapId | {MapId | TeamId}
handle_info({register_map, MapKey, MapNode, MapRegName}, State) ->
	ets:insert(ets_map_dispatch, {MapKey, {MapNode, MapRegName}}),
	{noreply, State};

% handle_info({register, RoleId, MapNode, MapRegName}, State) ->
% 	ets:insert(ets_map_dispatch, {RoleId, {MapNode, MapRegName}}),
% 	{noreply, State};

handle_info({'EXIT', _PID, Reason}, State) ->
    ?ERROR("~p recieve EXIT message, reason: ~p, state: ~p", [?MODULE, Reason, State]),
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(Reason, State) ->
    ?ERROR("~p terminate, reason: ~p, state: ~p", [?MODULE, Reason, State]),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.