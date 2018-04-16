%% @author dzR <dizengrong@gmail.com>
%% @doc 获取有关系统的信息

-module (util_sys).
-export([get_os_version/0]).

%% 获取操作系统的版本，如win32或者是linux
-spec get_os_version() -> string().
get_os_version() ->
	erlang:system_info(system_architecture).