-module (robot_util).
-compile([export_all]).
-include ("common.hrl").
-include ("log.hrl").

%% 发送协议消息
send_msg(Socket, Msg) ->
	Bin = pt:packet(Msg),
	?ERROR("Send packet: ~p", [Msg]),
	ok = gen_tcp:send(Socket, Bin).
	% erlang:port_command(Socket, Bin).

is_traced(State) ->
	State#robot.role_id == 1000587.

random(Min, Max)->
    Min2 = Min-1,
    random:uniform(Max-Min2)+Min2.

%% 从一个list中随机出一个元素
random_from_list(List) ->
	Nth = random(1, length(List)),
	lists:nth(Nth, List).

%% 获取从点Point1到Point2的直线所在的方向(这个方法是根据前端的as代码翻译过来的)
-spec get_direction({integer(), integer()}, {integer(), integer()}) -> 0|1|2|3|4|5|6|7.
get_direction(Point1, Point2) ->
	{X1, Y1} = Point1,
	{X2, Y2} = Point2,
	DiffX = X2 - X1,
	DiffY = Y1 - Y2,
	if
		DiffX /= 0 ->
			Var = erlang:abs(DiffY/DiffX),
			if
				Var < 0.41421356237309503 ->
					?_if(DiffX > 0, 2, 6);
				Var > 2.414213562373095 ->
					?_if(DiffY > 0, 0, 4);
				true ->
					if
						DiffX > 0 ->
							?_if(DiffY > 0, 1, 3);
						true ->
							?_if(DiffY > 0, 7, 5)
					end
			end;
		DiffY > 0 -> 0;
		true 	  -> 4
	end.

now() ->
    {A, B, _} = erlang:now(),
    A * 1000000 + B.
