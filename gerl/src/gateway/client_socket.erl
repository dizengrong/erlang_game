%% @author dzR <dizengrong@gmail.com>
%% @doc 处理与客户端的连接，接收数据并将消息路由出去

-module (client_socket).

-include("log.hrl").
-include("proto_login_pb.hrl").
-include("proto_map_pb.hrl").
-include ("gateway.hrl").
-include ("db_table_role.hrl").
-export([start_link/4, send_to_client/2, change_map/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% 	STATUS_PLAY <---已有角色--- STATUS_LOGIN  
%%				|				|
%%				|没有角色		|
%%				|----------> STATUS_REGISTER
%% 注意状态数字从小到大表示了状态的递进
-define(STATUS_LOGIN, 		1).		%% 等待登陆
-define(STATUS_REGISTER, 	2).		%% 等待注册
-define(STATUS_ENTER_GAME, 	3).		%% 等待进入开始游戏
-define(STATUS_ENTER_MAP, 	4).		%% 等待第一次进入地图
-define(STATUS_PLAY, 		5).		%% 已进入开始游戏了

-define(OK,								0).		%% 表示ok的返回码
-define(LOGIN_ACCOUNT_NOT_REGISTERED, 	1). 	%% 登陆验证账号没有注册

-record(state, {
	socket,
	transport, 			%% the ranch socket transport
	role_id,			%% 玩家id
	account_name, 		%% 账号
	status,				%% 玩家状态
	at_map_name			%% 所在地图进程信息:{注册名::MapRegName, 节点::MapNode} 
}).

%% ranch的回调方法
% start_link(Ref, Socket, Transport, _Opts) ->
% 	?INFO("Ref: ~p", [Ref]),
% 	true        = inet_db:register_socket(Socket, inet_tcp),
% 	StartFunc   = {?MODULE, start_link, [Ref, Socket, Transport]},
% 	Spec        = {Ref, StartFunc, temporary, brutal_kill, worker, [?MODULE]},
% 	{ok, Child} = gerl_gateway_sup:start_child(Spec),
% 	{ok, Child}.

%% gerl_gateway_sup的回调方法
start_link(Ref, Socket, Transport, _Opts) ->
	true        = inet_db:register_socket(Socket, inet_tcp),
    {ok, Pid} =  gen_server:start_link(?MODULE, [Ref, Socket, Transport], 
    		[{spawn_opt, [{min_heap_size, 10*1024},{min_bin_vheap_size, 10*1024}]}]),
    % gen_tcp:controlling_process(Socket, Pid),
    {ok, Pid}.

-spec send_to_client(RoleId::integer(),  Msg::tuple() | list()) -> any().
%% @doc 发送网络消息包给玩家
send_to_client(RoleId, Msg) ->
	case role_socket:lookup(RoleId) of
		[]    -> ignore;
		[Rec] -> Rec#role_socket.pid ! {packet_to_client, Msg}
	end.

-spec change_map(RoleId::integer(), 
				 MapNode::node(), 
				 MapRegName::atom()) -> {error, socket_process_down} | {ok, success}.
%% @doc 玩家改变所在的地图后同步更新socket进程的地图进程数据
change_map(RoleId, MapNode, MapRegName) -> 
	case role_socket:lookup(RoleId) of
		[]    -> {error, socket_process_down};
		[Rec] -> gen_server:call(Rec#role_socket.pid, {change_map, MapNode, MapRegName})
	end.

init([_Ref, Socket, Transport]) ->
	inet:setopts(Socket, [{packet, 4}, binary, {active, false}, {nodelay, true}, {delay_send, true}]),
	erlang:process_flag(trap_exit, true),
	% ok = ranch:accept_ack(Ref),
	up_socket_watermark(Socket),
	State = #state{
		socket    = Socket, 
		transport = Transport,
		status    = ?STATUS_LOGIN
	},
	{ok, State}.

up_socket_watermark(Socket) -> 
	{ok, [{high_watermark, High}]} = inet:getopts(Socket, [high_watermark]),
	{ok, [{low_watermark, Low}]}   = inet:getopts(Socket, [low_watermark]),
	inet:setopts(Socket, [{high_watermark, High*2}, {low_watermark, Low*2}]).

open_recv(Socket) ->
	prim_inet:async_recv(Socket, 0, -1).

%% ranch发送消息shoot过来后表示一切就绪，开启接收数据
handle_info({shoot, _RanchRef}, #state{socket = Socket} = State) ->
	open_recv(Socket),
    {noreply, State};
%% 接收数据
handle_info({inet_async, Socket, _Ref, {ok, Data}}, State)
	when State#state.status == ?STATUS_ENTER_MAP -> 
	{MsgCode, Msg} = util_packet:unpacket(Data),
	case is_record(Msg, ms_map_enter) of
		true -> %% 这里就是登陆游戏并第一次进入地图了，即真正上线了
			RoleId = State#state.role_id,
			online_srv:role_online(RoleId),
			role_socket:insert(RoleId, Socket, State#state.transport, self()),
			router(State, MsgCode, Msg),
			event_role_online(State),
			open_recv(State#state.socket),
			{noreply, State#state{status = ?STATUS_PLAY}};
		false ->
			{stop, {shutdown, {error_not_ms_map_enter, Msg}}, State}
	end;
handle_info({inet_async, _Socket, _Ref, {ok, Data}}, State)
	when State#state.status == ?STATUS_PLAY -> 
	{MsgCode, Msg} = util_packet:unpacket(Data),
	router(State, MsgCode, Msg),
	open_recv(State#state.socket),
	{noreply, State};
handle_info({inet_async, _Socket, _Ref, {ok, Data}}, State) ->
	{_, Msg} = util_packet:unpacket(Data),
	State2 = if
		is_record(Msg, ms_login) 	  -> do_login(State, Msg);
		is_record(Msg, ms_register)   -> do_register(State, Msg);
		is_record(Msg, ms_enter_game) -> do_enter_game(State, Msg);
		true -> 
			?WARNING("Msg: ~p unhandled\n", [Msg]),
			State
	end,
	open_recv(State#state.socket),
	{noreply, State2};
handle_info({inet_async, _Socket, _Ref, {error, closed}}, State) ->
	{stop, {shutdown, closed}, State};
%% 客户端超时
handle_info({inet_async, _Socket, _Ref, {error, timeout}}, State) ->
	{stop, {shutdown, timeout}, State};
handle_info({'EXIT', _, Reason}, State) ->
	{stop, {shutdown, Reason}, State};

handle_info({packet_to_client, Msg}, State) ->
	util_packet:send(State#state.transport, State#state.socket, Msg),
	{noreply, State};
handle_info(Request, State) ->
	?WARNING("info request: ~p unhandled\n", [Request]),
	{noreply, State}.

handle_cast(Request, State) ->
	?WARNING("cast request: ~p unhandled\n", [Request]),
	{noreply, State}.

handle_call({change_map, MapNode, MapRegName}, _From, State) ->
	NewState = State#state{at_map_name = {MapRegName, MapNode}},
	{reply, {ok, success}, NewState};

handle_call(Request, From, State) ->
	?ERROR("Unknown request:~p from ~p", [Request, From]),
	{reply, {error, unknown_call}, State}.

code_change(_, _, _) -> {ok, ok}.

terminate(Reason, State) -> 
	close_client(State, Reason),
	ok.

%% @doc 协议消息路由
%% MsgCode为协议消息号，Msg为具体的协议消息
router(State, MsgCode, Msg) ->
	RoleId = State#state.role_id,
	case MsgCode div 1000 of
		12 -> chat_api:chat(RoleId, Msg);
		_  -> router_to_map(State, {packet, RoleId, MsgCode, Msg})
	end.

router_to_map(State, Msg) -> erlang:send(State#state.at_map_name, Msg).

close_client(State, Reason) ->
	case State#state.status >= ?STATUS_ENTER_MAP of
		true ->
			RoleId = State#state.role_id,
			event_role_offline(State),
			online_srv:role_offline(RoleId),
			role_socket:delete(RoleId),
			erlang:port_close(State#state.socket);
		false ->
			ignore
	end,
	?INFO("close client for reason: ~p, state: ~p", [Reason, State]),
	ok.

do_login(State, Msg) when State#state.status == ?STATUS_LOGIN -> 
	AccountName = Msg#ms_login.account_name,
	State1      = State#state{account_name = AccountName},
	case account_srv:get_role_id_by_account(AccountName) of
		0 ->
			Msg2   = #mc_login{ret_code = ?LOGIN_ACCOUNT_NOT_REGISTERED},
			State2 = State1#state{status = ?STATUS_REGISTER};
		RoleId ->
			Msg2   = #mc_login{ret_code = ?OK, role_id = RoleId},
			State2 = State1#state{role_id = RoleId, status = ?STATUS_ENTER_GAME}
	end,
	util_packet:send(State2#state.transport, State2#state.socket, Msg2),
	State2;
do_login(State, _Msg) -> 
	erlang:exit({wrong_status_when_login, State#state.status}),
	State.

do_register(State, Msg) when State#state.status == ?STATUS_REGISTER ->
	AccountName = State#state.account_name,
	RoleName    = Msg#ms_register.role_name,
	HeroId    = Msg#ms_register.hero_id,
	case account_srv:register_account(AccountName, RoleName, HeroId) of
		{ok, NewRoleId} -> 
			Msg2   = #mc_register{ret_code = ?OK, role_id = NewRoleId},
			State2 = State#state{status = ?STATUS_ENTER_GAME, role_id = NewRoleId};
		{error, RetCode} ->
			Msg2   = #mc_register{ret_code = RetCode},
			State2 = State
	end,
	util_packet:send(State2#state.transport, State2#state.socket, Msg2),
	State2;
do_register(State, _Msg) ->
	erlang:exit({wrong_status_when_register, State#state.status}),
	State.

do_enter_game(State, _Msg) when State#state.status == ?STATUS_ENTER_GAME ->
	%% TODO:在这里将客户端需要的数据都发给它
	RoleId     = State#state.role_id,
	RolePosRec = db_role:get_role_pos_rec(RoleId),
	MapId      = RolePosRec#tab_role_pos.cur_mapid,
	Msg2       = #mc_enter_game{
		role_id = RoleId, 
		map_id  = MapId, 
		x       = RolePosRec#tab_role_pos.cur_x, 
		y       = RolePosRec#tab_role_pos.cur_y
	},
	util_packet:send(State#state.transport, State#state.socket, Msg2),

	{MapNode, MapRegName} = map_dispatch:query_map_name(MapId),
	State#state{status = ?STATUS_ENTER_MAP, at_map_name = {MapRegName, MapNode}};
do_enter_game(State, _Msg) ->
	erlang:exit({wrong_status_when_enter_game, State#state.status}),
	State.


%% ============================= gateway事件处理 ==================================
event_role_online(State) ->
	RoleId = State#state.role_id,
	try
		chat_api:join_p2p(RoleId),
		chat_api:join_world(RoleId),
		ok
	catch
		Type:Reason -> ?PRINT_STACKTRACE(event_role_online, Type, Reason)
	end.

event_role_offline(State) ->
	RoleId = State#state.role_id,
	try
		router_to_map(State, {gateway_role_offline, RoleId}),
		chat_api:leave_p2p(RoleId),
		chat_api:leave_world(RoleId),
		ok
	catch
		Type:Reason -> ?PRINT_STACKTRACE(event_role_offline, Type, Reason)
	end.

