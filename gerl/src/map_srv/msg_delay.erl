%% @author dzR <dizengrong@gmail.com>
%% @doc 性能较好的消息延迟发送处理
%% 这里借鉴的是scalaris中的msg_delay模块的思想，其说明如下：
%% Instead of using send_after, which is slow in Erlang, as it
%% performs a system call, this module allows for a weaker
%% message delay.
%% You can specify the minimum message delay in seconds and
%% send the message sometime afterwards.
%% Internally it triggers itself periodically to schedule sending.

%% 这里我的实现将明确是在地图进程中

-module (msg_delay).
-include("spec_type.hrl").
-export([init/0, one_second_loop/0]).
-export([send/3]).

init() ->
	init_counter().

%% @doc 地图的每秒定时器
one_second_loop() ->
	Counter    = get_counter(),
	NewCounter = Counter + 1,
	set_counter(NewCounter),

	case get_delay_msg_queue(NewCounter) of
		undefined -> ok;
		MsgQueue  -> [erlang:send(Dest, Msg) || {Dest, Msg} <- MsgQueue]
	end,
	delete_delay_msg_queue(Counter),
	ok.

-spec send(Seconds::non_neg_integer(), Dest::dest(), Msg::tuple()) -> any().
%% @doc 发送延迟消息
%% Seconds为延迟多少秒，Dest同erlang:send/2中的Dest，Msg为要发送的消息
send(Seconds, Dest, Msg) ->
	Counter = get_counter(),
	Future  = trunc(Counter + Seconds),
	case get_delay_msg_queue(Counter) of
		undefined -> set_delay_msg_queue(Future, [{Dest, Msg}]);
		MsgQueue  -> set_delay_msg_queue(Future, [{Dest, Msg} | MsgQueue])
	end.

get_delay_msg_queue(Counter) ->
	map_dict:get({?MODULE, delay_queue, Counter}).
set_delay_msg_queue(Future, MsgQueue) ->
	map_dict:set({?MODULE, delay_queue, Future}, MsgQueue).
delete_delay_msg_queue(Counter) ->
	map_dict:delete({?MODULE, delay_queue, Counter}).

init_counter() ->
	set_counter(0).
get_counter() ->
	map_dict:get({?MODULE, counter}).
set_counter(Counter) ->
	map_dict:set({?MODULE, counter}, Counter).

