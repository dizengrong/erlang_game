%% @author dzR <dizengrong@gmail.com>
%% @doc 玩家的循环

-module (role_loop).
-include ("log.hrl").
-include ("common.hrl").

-export ([one_second_loop/2]).

%% @doc 玩家的每秒循环
one_second_loop(RoleId, NowSeconds) ->
	try
		?_IF(NowSeconds rem 3 == 0, item_life:check_lifetime(RoleId, NowSeconds)),
		ok
	catch
		Type:Reason -> ?PRINT_STACKTRACE(one_second_loop, Type, Reason)
	end.
