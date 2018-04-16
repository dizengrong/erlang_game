%% @author dzR <dizengrong@gmail.com>
%% @doc 这个是服务器启动和停止的总入口

-module (main).
-export([start/1, stop/0, info/0]).

%% @doc 启动所有app
start([ServerIndex]) ->
	{ok, File} = gen_gerl_setting:generate(list_to_integer(atom_to_list(ServerIndex))),
	c:c(File),
	Apps = gerl_setting:get({start_apps, node()}),
	ok = lager:start(),	%% 启动日志app
	[ok = application:start(App) || App <- Apps],
	ok.

%% @doc 关闭系统
stop() ->
	Apps = gerl_setting:get({start_apps, node()}),
	[ok  = application:stop(App) || App <- Apps],
	ok   = application:stop(lager),
	init:stop(),
	ok.


%% @doc 获取运行时的一些系统数据
info() ->
	SchedId      = erlang:system_info(scheduler_id),
	SchedNum     = erlang:system_info(schedulers),
	ProcCount    = erlang:system_info(process_count),
	ProcLimit    = erlang:system_info(process_limit),
	ProcMemUsed  = erlang:memory(processes_used),
	ProcMemAlloc = erlang:memory(processes),
	MemTot       = erlang:memory(total),
	io:format("runtime information:
					   ~n   Scheduler id:                         ~w
					   ~n   Num scheduler:                        ~w
					   ~n   Process count:                        ~w
					   ~n   Process limit:                        ~w
					   ~n   Memory used by erlang processes:      ~w
					   ~n   Memory allocated by erlang processes: ~w
					   ~n   The total amount of memory allocated: ~w
					   ",
			[SchedId, SchedNum, ProcCount, ProcLimit, ProcMemUsed, ProcMemAlloc, MemTot]),
	  ok.