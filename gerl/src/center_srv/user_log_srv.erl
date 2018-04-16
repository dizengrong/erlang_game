%% @author dzR <dizengrong@gmail.com>
%% @doc 用户日志模块，用来记录道具、元宝以及其他数据的日志

-module (user_log_srv).
-include ("log.hrl").

-export([start_link/0, add_log/1]).

%% 内部使用导出
-export([add_log2/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, {global, ?MODULE}).

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, {}, []).

%% @doc 增加一条日志记录
add_log(LogRec) ->
	gen_server:cast(?SERVER, {add_log2, {LogRec}}).


init({}) ->
    erlang:process_flag(trap_exit, true),
	{ok, undefined}.

handle_call(Request, _From, State) ->
	{reply, {?MODULE, unknown_call, Request}, State}.

handle_cast({Fun, Arg}, State) ->
	Fun(Arg),
	{noreply, State}.

handle_info({'EXIT', _PID, Reason}, State) ->
    ?ERROR("~p recieve EXIT message, reason: ~p, state: ~p", [?MODULE, Reason, State]),
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, State) ->
    ?ERROR("~p terminate, reason: ~p, state: ~p", [?MODULE, Reason, State]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% @doc todo:目前记录用户日志使用的数据库就是和玩家数据库，以后可能会该
add_log2({LogRec}) ->
	db:add_new(LogRec).
