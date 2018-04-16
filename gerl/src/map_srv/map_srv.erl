%% @author dzR <dizengrong@gmail.com>
%% @doc 地图服务进程，玩家的所有游戏逻辑都在地图里进行处理

-module (map_srv).
-include("log.hrl").
-include("map.hrl").
-include("common.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, lock_packet/1]).
-export([debug/2]).

%% 地图进程的性能调优参数
-define(MAP_PROCESS_OPT,  [{spawn_opt, [{min_heap_size, 1024*1024}, 
										{min_bin_vheap_size, 4*1024*1024}]}]).

-spec start_link(MapId::integer(), RegName::atom()) -> {ok, pid()}.
%% @doc 创建一个地图进程
%% RegName:地图进程注册的名字
start_link(MapId, RegName) ->
	gen_server:start_link({local, RegName}, ?MODULE, {MapId, RegName}, ?MAP_PROCESS_OPT).

%% @doc 用于在地图进程里同步执行某个方法的调试接口
%% MapKey与map_dispatch:query_map_name/1方法的参数一致
debug(MapKey, Fun) ->
	case map_dispatch:query_map_name(MapKey) of
		{MapNode, RegName} -> gen_server:call({RegName, MapNode}, {debug, Fun});
		_ -> ?ERROR("Cannot find map by map key: ~p", [MapKey])
	end.

%% @private
init({MapId, RegName}) ->
	erlang:process_flag(trap_exit, true),
	%% 缓存时间now，避免对时间的多次调用，那样效率低
	map_dict:set_now_seconds(util_time:now_seconds()),
	State = #map_state{map_id = MapId, reg_name = RegName, tick = 0},
	set_map_state(State),
	msg_delay:init(),
	map_dict:init(State),
	npc_dict:init(State),
	map_event:init(State),
	npc:init(State),
	erlang:send_after(?MAIN_LOOP_INTERVAL, self(), main_loop),
	{ok, State}.

%% @private
handle_call({debug, Fun}, _From, State) ->
	{reply, Fun(), State};
handle_call(Request, _From, State) ->
	?ERROR("Unhandled message: ~p", [Request]),
	{reply, {error, unknown_call}, State}.

%% @private
handle_cast({role_offline, RoleId}, State) ->
	ets:delete(ets_online, {RoleId}),
	{noreply, State}.

%% @private
handle_info({'EXIT', _PID, Reason}, State) ->
	?ERROR("~p recieve EXIT message, reason: ~p, state: ~p", [?MODULE, Reason, State]),
	{stop, normal, State};

%% @doc 地图主定时器循环
handle_info(main_loop, State) ->
	erlang:send_after(?MAIN_LOOP_INTERVAL, self(), main_loop),
	Tick  = State#map_state.tick + 1,
	map_event:main_loop(Tick),
	NowSeconds = map_dict:get_now_seconds(),
	?_IF(Tick rem 5 == 0, map_event:one_second_loop(State, NowSeconds)),
	%% 每分钟重新设置下now_seconds，使其保持准确的值
	Tick2 = Tick rem 300,
	?_IF(Tick2 == 0, map_dict:set_now_seconds(util_time:now_seconds())),
	{noreply, State#map_state{tick = Tick}};

handle_info(Request, State) ->
	try
		do_handle_info(Request, State)
	catch
		Type:Reason -> ?PRINT_STACKTRACE(Request, Type, Reason)
	end,
	{noreply, State}.

%% @private
terminate(Reason, State) ->
	?ERROR("~p terminate, reason: ~p, state: ~p", [?MODULE, Reason, State]),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% @doc 客户端协议包
do_handle_info({packet, RoleId, MsgCode, Msg}, State) ->
	case is_packet_locked(RoleId) of
		true  -> ignore;
		false ->
			CodeTag = protocol_code:code_tag(c_2_s, MsgCode),
			case MsgCode div 1000 of
				11 -> pp_map:client_reqeust(CodeTag, RoleId, Msg, State)
			end
	end;

%% 从网关发来的玩家下线的消息
do_handle_info({gateway_role_offline, RoleId}, _State) ->
	map_api:role_leave(RoleId),
	ok;

do_handle_info(Request, _State) ->
	?ERROR("unhandled reqeust: ~p", [Request]).

set_map_state(State) -> erlang:put(map_state, State).

%% @doc 锁定客户端的协议包不做路由和响应处理
%% 在这些情况下要进行锁定：
%% 		客户端成功请求跳转地图了
lock_packet(RoleId) -> role_dict_tmp:set(RoleId, lock_packet, true).
is_packet_locked(RoleId) -> role_dict_tmp:get(RoleId, lock_packet) == true.