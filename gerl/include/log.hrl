%% @doc 有关日志函数的宏定义

%% 这个是便于调试用的
-define(DBG(Args), ?ERROR("DBG ~p==~p",[??Args, Args])).

%% 以下定义是按照从低到高的优先级定义的
-define(DEBUG(Format), lager:debug(Format)).
-define(DEBUG(Format, Args), lager:debug(Format, Args)).

-define(INFO(Format), lager:info(Format)).
-define(INFO(Format, Args), lager:info(Format, Args)).

-define(WARNING(Format), lager:warning(Format)).
-define(WARNING(Format, Args), lager:warning(Format, Args)).

-define(ERROR(Format), lager:error(Format)).
-define(ERROR(Format, Args), lager:error(Format, Args)).

-define(CRITICAL(Format), lager:critical(Format)).
-define(CRITICAL(Format, Args), lager:critical(Format, Args)).

-define(ALERT(Format), lager:alert(Format)).
-define(ALERT(Format, Args), lager:alert(Format, Args)).

-define(EMERGENCY(Format), lager:emergency(Format)).
-define(EMERGENCY(Format, Args), lager:emergency(Format, Args)).

-define(PRINT_STACKTRACE(Request, T, R), 
    	?ERROR("Request:~w~nType:~w~nReason:~w~nstack:~100p", 
    			[Request, T, R, erlang:get_stacktrace()])).
