%% @author dzR <dizengrong@gmail.com>
%% @doc 网关发送器
%% 玩家登陆之后，发给玩家的消息都通过这个来转发给client_socket模块
%% 不过对于广播类消息则直接由本进程来执行发送了，
%% 这样就避免了要复制N份消息再转发了
%% 同时也避免了消息的集中处理

-module (gateway_dispatch).
-include ("gateway.hrl").
-include ("log.hrl").

-export([start_link/0, send_to_role/2, change_map/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, {global, ?MODULE}).

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, {}, []).

-spec send_to_role(RoleId::integer(), Msg::tuple() | list()) -> any().
%% @doc 发送网络消息包给玩家(个人消息只做转发)
send_to_role(RoleId, Msg) ->
	gen_server:cast(?SERVER, {send_to_role, RoleId, Msg}).

-spec change_map(RoleId::integer(), 
				 MapNode::node(), 
				 MapRegName::atom()) -> {error, socket_process_down} | {ok, success}.
%% @doc 玩家改变所在的地图后同步更新socket进程的地图进程数据
change_map(RoleId, MapNode, MapRegName) ->
	gen_server:call(?SERVER, {change_map, RoleId, MapNode, MapRegName}).

%% @private
init({}) ->
    erlang:process_flag(trap_exit, true),
	{ok, undefined}.

%% @private
handle_call({change_map, RoleId, MapNode, MapRegName}, _From, State) ->
	Reply = client_socket:change_map(RoleId, MapNode, MapRegName),
	{reply, Reply, State};
handle_call(Request, From, State) ->
	?ERROR("Unknown request:~p from ~p", [Request, From]),
	{reply, {error, unknown_call}, State}.

handle_cast({send_to_role, RoleId, Msg}, State) ->
	client_socket:send_to_client(RoleId, Msg),
	{noreply, State}.

%% @private
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
code_change(_OldVsn, State, _Extra) -> {ok, State}.

