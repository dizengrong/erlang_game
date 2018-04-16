%% @doc 生成运行需要的setting文件

-module (gen_gerl_setting).
-compile([export_all]).


generate(ServerIndex) ->
	File     = "run_setting/gerl_setting.erl",
	Mod      = list_to_atom("gerl_setting" ++ integer_to_list(ServerIndex)),
	{ok, Fd} = file:open(File, write),
	io:format(Fd, content(), [Mod]),
	file:close(Fd),
	{ok, File}.


content() ->
"%% @author dzR <dizengrong@gmail.com>
%% @doc some system setting(This file is generated automaticly)

-module (gerl_setting).
-compile([export_all]).

get(Key) -> ~p:get(Key).".	