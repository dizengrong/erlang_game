%% @author dzR <dizengrong@gmail.com>
%% @doc 全局的账号服务进程，管理账号的注册和查询

-module (online_srv).
-include("log.hrl").
-include("spec_type.hrl").

-export([start_link/0, role_online/1, role_offline/1]).
-export([is_online/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, {global, ?MODULE}).

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, {}, []).

%% @doc 玩家上线登记
role_online(RoleId) ->
	gen_server:cast(?SERVER, {role_online, RoleId}).

%% @doc 玩家下线登记
role_offline(RoleId) ->
	gen_server:cast(?SERVER, {role_offline, RoleId}).

-spec is_online(RoleId::role_id()) -> boolean().
%% @doc 查询玩家是否在线
is_online(RoleId) ->
	gen_server:call(?SERVER, {is_online, RoleId}).

%% @private
init({}) ->
    erlang:process_flag(trap_exit, true),
	{ok, undefined}.

%% @private
handle_call({is_online, RoleId}, _From, State) ->
	{reply, ets:lookup(ets_online, RoleId) /= [], State}.

%% @private
handle_cast({role_online, RoleId}, State) ->
	ets:insert(ets_online, {RoleId}),
	{noreply, State};

handle_cast({role_offline, RoleId}, State) ->
	ets:delete(ets_online, {RoleId}),
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
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
