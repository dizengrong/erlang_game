-module (client_robot).
-compile([export_all]).
-include ("common.hrl").
-include ("map.hrl").
-include ("log.hrl").
-include ("proto_map_pb.hrl").
-include ("proto_login_pb.hrl").


start_link(Ip, Port, Accout) ->
	gen_server:start_link(?MODULE, [Ip, Port, Accout], []).

init([Ip, Port, Accout]) ->
	?DBG(Accout),
	erlang:process_flag(trap_exit, true),
	Ai = robot_util:random_from_list(?ALL_AI_TYPES),
	mod_monster:init(),
	{ok, Socket} = gen_tcp:connect(Ip, Port, [{packet, 4}, binary, {active, true}, {nodelay, true}, {delay_send, true}]),
	Robot = #robot{
		status  = ?ROBOT_STATUS_LOGIN,
		sokcet  = Socket,
		account = Accout,
		ai      = Ai
	},
	erlang:send(self(), one_second_loop),
	ets:insert(tab_account, Robot),
	erlang:send_after(5000, self(), do_after_login),
	random:seed(now()),
	{ok, Robot}.

handle_info(do_after_login, State) ->
	{noreply, State};

handle_info(end_cast_last_skill, State) ->
	{noreply, State#robot{status = ?ROBOT_STATUS_IDLE}};
handle_info(one_second_loop, State) ->
	case robot_util:is_traced(State) of
		true ->
			[OldState] = ets:lookup(tab_account, State#robot.account),
			io:format("~w --> ~w\n", [OldState#robot.status, State#robot.status]);
		_ -> ignore
	end,
	ets:insert(tab_account, State),
	erlang:send_after(1000, self(), one_second_loop),
	NewState = case State#robot.status of
		?ROBOT_STATUS_LOGIN ->
			do_login(State);
		?ROBOT_STATUS_REGISTER_ROLE ->
			do_login_register_role(State);
		?ROBOT_STATUS_ENTER_GAME ->
			do_enter_game(State);
		?ROBOT_STATUS_ENTER_MAP ->
			do_enter_map(State);
		?ROBOT_STATUS_WAITING -> 
			State;
		?ROBOT_STATUS_IDLE -> 
			do_role_idle(State),
			do_execute_ai(State);
		?ROBOT_STATUS_WALKING ->
			do_walk(State);
		?ROBOT_STATUS_CAST_LAST_SKILL ->
			State;
		Status ->
			io:format("unhandled robot status: ~w\n", [Status]),
			State
	end,
	{noreply, NewState};


handle_info({do_change_state, Status}, State) ->
	{noreply, State#robot{status = Status}};

handle_info({inet_reply, _Sock, ok}, State) ->
    {noreply, State};
handle_info({inet_reply, _Sock, Result}, State) ->
    io:format("ERR~ts:~p", ["socket发送结果", Result]),
    {stop, normal, State};

handle_info({inet_async, _Socket, _Ref, {ok, Data}}, State) ->
	io:format("inet_async11\n"),
	NewState = handle_receive_msg(pt:unpacket(Data), State),
	{noreply, NewState};

handle_info({inet_async, _Socket, _Ref, {error, closed}}, State) ->
	io:format("The socket of account ~s is closed, process exit!", [State#robot.account]),
	{stop, normal, State};

handle_info({tcp, _Socket, InData}, State) ->
	NewState = handle_receive_msg(pt:unpacket(InData), State),
	{noreply, NewState};
handle_info({tcp_error, _Socket, Reason}, State) ->
	io:format("Tcp error, reason: ~p, process exit!\n", [Reason]),
	{stop, normal, State};
handle_info({tcp_closed, _Socket}, State) ->
	io:format("Tcp is closed, process exit!\n", []),
	{stop, normal, State};

handle_info({run_fun, Fun}, State) ->
	Fun(),
	{noreply, State};

handle_info({move_walk, WalkInterval}, State) ->
	WalkPath = State#robot.walk_path,
	NewState = case WalkPath of
		[] -> State#robot{status = ?ROBOT_STATUS_IDLE};
		_ ->
			[{X, Y} | LeftPath] = WalkPath,
			% io:format("pos:~w\n", [{X, Y}]),
			Msg = #ms_map_move_check{x = X, y = Y},
			robot_util:send_msg(State#robot.sokcet, Msg),
			
			erlang:send_after(WalkInterval, self(), {move_walk, WalkInterval}),
			State#robot{pos = State#robot.pos#r_pos{x = X, y = Y}, walk_path = LeftPath}
	end,
	{noreply, NewState};

handle_info({'EXIT', _, Reason}, State) ->
	gen_tcp:close(State#robot.sokcet),
	{stop, Reason, State}.


terminate(Reason, State) ->
	gen_tcp:close(State#robot.sokcet),
	io:format("Client robot terminate for reason: ~p\n", [Reason]),
    ok.

do_login(State) ->
	?DBG(State),
	Msg = #ms_login{account_name = State#robot.account},
	robot_util:send_msg(State#robot.sokcet, Msg),
	State#robot{status = ?ROBOT_STATUS_WAITING}.

sex(2) -> 2;
sex(_) -> 1.

do_login_register_role(State) ->
	?DBG(State),
	HeroId = robot_util:random(1, 3), %% 职业随机
	% Category = 1,
	Msg = #ms_register{
		hero_id   = HeroId,
		role_name = State#robot.account
	},
	robot_util:send_msg(State#robot.sokcet, Msg),
	State#robot{status = ?ROBOT_STATUS_WAITING}.

do_enter_map(State) ->
	Msg = #ms_map_enter{map_id = State#robot.map_id},
	robot_util:send_msg(State#robot.sokcet, Msg),
	NewState = State#robot{status = ?ROBOT_STATUS_WAITING},
	NewState.

do_enter_game(State) ->
	Msg      = #ms_enter_game{role_id = State#robot.role_id},
	robot_util:send_msg(State#robot.sokcet, Msg),
	NewState = State#robot{status = ?ROBOT_STATUS_WAITING},
	NewState.

%% 执行机器人的ai
do_execute_ai(State) ->
	case State#robot.ai of
		?AI_TYPE_ATTACK_MONSTER -> ai_attack:handle(State)
	end.

%% 空闲状态处理(注意这里不会返回State的，对State的修改不会保存的)
do_role_idle(_State) ->
	% case application:get_env(robot, action) of
	% 	{ok, action_kill_dragon} ->
	% 		robot_action:action_kill_dragon(State);
	% 	_ -> ignore
	% end,
	ok.

%% 走路处理
do_walk(State) ->
	case State#robot.walk_path of
		[] -> State#robot{status = ?ROBOT_STATUS_IDLE};
		WalkPath ->
			Msg = #ms_map_move{
				path = lists:flatten([[X, Y] || {X, Y} <- WalkPath])
			},
			robot_util:send_msg(State#robot.sokcet, Msg),
			erlang:send(self(), {move_walk, 200}),
			State#robot{status = ?ROBOT_STATUS_WAITING}
	end.


%% 处理收到的网络消息
handle_receive_msg({MsgCode, Msg}, State) ->
	%% mod_xxx必须要返回一个新的State
	NewState = case MsgCode div 1000 of
		10 		-> mod_role:handle(Msg, State);
		11 		-> mod_map:handle(Msg, State);
		_Mod  	    -> 
			% io:format("module ~w message is not handle\n", [Mod]),
			State
	end,
	NewState.
