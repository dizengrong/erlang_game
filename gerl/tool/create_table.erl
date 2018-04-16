%% @doc 使用amnesia来创建数据库

-module (create_table).
-compile([export_all]).
-include("db.hrl").

%% 生成db_tables.erl文件，并生成一个数据库和所有的表
create_one() ->
	{ok, Mod} = create_file(),
	os:cmd("rebar compile"),
	c:l(Mod),
	amnesia:db_tool(Mod, get_make_db_conf()),
	ok=file:rename(lists:concat(["include/", Mod, ".hrl"]), "include/db_tables.hrl"),
	ok.

%% 生成不同的数据库和头文件
create_all() ->
	Files = filelib:wildcard("src/db/db_table_*.erl"),
	create_table_help(Files),
	ok.

create_table_help([]) -> ok;
create_table_help([File | Rest]) ->
	TableModule = list_to_atom(filename:basename(File, ".erl")),
	%% 下面的sleep和开一个进程的原因是amnesia:db_tool中打开了mysql连接
	%% 但关闭时貌似有问题，关闭的消息会发送给上一次打开的那个进程。。。
	timer:sleep(1000),
	spawn(fun() ->
		io:format("Create tables from: ~p\n", [TableModule]), 
		amnesia:db_tool(TableModule, get_make_db_conf()) 
	end),
	create_table_help(Rest),
	ok.

get_make_db_conf() ->
	case util_sys:get_os_version() of
		"win32" -> %% windows下创建数据库由于环境的原因会找不到mysqladmin命令，所以需要手动创建
			[{make_hdr, "include"}, make_sql, {dba_user, ?DB_USER}, {dba_password, ?DB_PASS}];
		_ ->
			[{make_hdr, "include"}, make_db, {dba_user, ?DB_USER}, {dba_password, ?DB_PASS}]
	end.

create_file() ->
	ServerIndex = gerl_setting:get(server_index),
	Mod = list_to_atom("gerl_db" ++ erlang:integer_to_list(ServerIndex)),
	File = "src/db/gerl_db" ++ erlang:integer_to_list(ServerIndex) ++ ".erl",
	io:format("Create tables from: ~p\n", [File]),
	{TabNameList, TabDefList} = get_all_tables(),
	create_db_mod_file(TabNameList),
	TabNameList2 = [Tab || {Tab, _} <- TabNameList],
	FunTables = "tables() -> " ++ util_str:term_to_str(TabNameList2) ++ ".\n\n",
	io:format("~p~n", [TabDefList]),
	Fun = fun({TabName, TabDef}) ->
		"table(" ++ atom_to_list(TabName) ++ ") -> \n\t" ++ util_str:term_to_str(TabDef) ++ ";\n"
	end,
	FunTabDefs = [Fun(T) || T <- TabDefList],
	{ok, Fd} = file:open(File, write),
	Head = io_lib:format(file_head(), [Mod]),
	io:format (Fd, "~s~n", [Head ++ FunTables ++ FunTabDefs ++ file_foot()]),
	file:close(Fd),
	{ok, Mod}.

create_db_mod_file(TabNameList) ->
	Head = "%% @author dzR <dizengrong@gmail.com>
%% @doc map each table to the right db_table_xxx
%% This file is generated automaticly 
-module (~s).
-compile([export_all]).

",
	Mod = "db_module",
	File = "src/db/" ++ Mod ++ ".erl",
	{ok, Fd} = file:open(File, write),
	Fun = fun(TabName, DbMod) ->
		"tab2db_mod(" ++ atom_to_list(TabName) ++ ") -> " ++ util_str:term_to_str(DbMod) ++ ";\n"
	end,
	Content = [Fun(T, M) || {T, M} <- TabNameList],
	io:format (Fd, Head, [Mod]),
	io:format (Fd, "tables() -> ~s.\n\n", [util_str:term_to_str([T || {T, _M} <- TabNameList])]),
	io:format (Fd, "~s\n", [Content]),
	io:format (Fd, "tab2db_mod(Tab) -> erlang:throw({?MODULE, error_confg, 'tab2db_mod/1', Tab}).\n", []),
	file:close(Fd).

get_all_tables() ->
	get_all_tables(filelib:wildcard("src/db/db_table_*.erl"), [], []).
get_all_tables([], TabNameList, TabDefList) -> 
	{lists:reverse(TabNameList), lists:reverse(TabDefList)};
get_all_tables([File | Rest], TabNameList, TabDefList) ->
	Mod          = list_to_atom(filename:basename(File, ".erl")),
	TabNameList1 = [{Tab, Mod} || Tab <- Mod:tables()] ++ TabNameList,
	TabDefList1  = [{Tab, Mod:table(Tab)} || Tab <- Mod:tables()] ++ TabDefList,
	get_all_tables(Rest, TabNameList1, TabDefList1).



file_head() ->
"%% @author dzR <dizengrong@gmail.com>
%% @doc We use amnesia([http://amnesia.sourceforge.net]) deal with mysql datas
%% This file is generated automaticly

-module (~p).
-compile([export_all]).
-include(\"db.hrl\").


driver_info () ->
	db_conf:driver_info().

".

file_foot() ->
	"table(Tab) -> erlang:throw({?MODULE, error_confg, 'table/1', Tab}).".