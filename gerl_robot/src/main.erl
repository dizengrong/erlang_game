-module (main).
-compile([export_all]).
-include ("common.hrl").

start() ->
	ok = lager:start(),	%% 启动日志app
	ok = application:start(sasl),
	ok = application:start(robot),
	ok.