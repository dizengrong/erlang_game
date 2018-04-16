%% @author dzR <dizengrong@gmail.com>
%% @doc 单独从数据库中数据的一些封装方法
%% 与gen_cache是独立的，一般数据都应该从gen_cache中获取的，
%% 除非你知道自己是在干啥，不然请不要乱用

-module (db).
-include("db_table_account.hrl").
-include("db.hrl").

-export ([init_amnesia_database/0]).
-export ([fetch/2, add_new/1, update/1, delete/1]).
-export ([get_all_accounts/0, get_max_role_id/0]).

-define(AMNESIA_DB_PID, 	global:whereis_name(amnesia_db)).
-define(AMNESIA_DB_NAME, 	erlang:list_to_existing_atom(?DB_NAME)).

%% 初始化amnesia数据库，即打开一个global数据库进程，这个进程由gerl_cache_app来管理
init_amnesia_database() ->
	case ?AMNESIA_DB_PID of
		undefined -> 
			{ok, Pid} = amnesia:open({global, amnesia_db}, ?AMNESIA_DB_NAME),
			{ok, Pid};
		Pid ->
			{ok, Pid}
	end.

-spec get_all_accounts() -> [#tab_account{}].
get_all_accounts() ->
	{ok, Records} = amnesia:fetch(?AMNESIA_DB_PID, tab_account),
	Records.

-spec get_max_role_id() -> non_neg_integer().
%% @doc 获取tab_account表中当前最大的role_id，没有记录的话则返回0
get_max_role_id() ->
	{ok, [MaxRoleId]} = amnesia:fetch(?AMNESIA_DB_PID, tab_account, {}, 
		[{aggregate, "IFNULL(max(role_id), 0)", integer}]),
	MaxRoleId.

-spec fetch(TableName::atom(), Predicate::tuple()) -> list().
%% @doc 获取TableName表中满足条件Predicate的数据
fetch(TableName, Predicate) ->
	{ok, Records} = amnesia:fetch(?AMNESIA_DB_PID, TableName, Predicate),
	Records.

-spec add_new(Rec::tuple()) -> {ok, tuple()}.
%% @doc 向数据库中插入数据
add_new(Rec) ->
	amnesia:add_new(?AMNESIA_DB_PID, Rec).

-spec update(Rec::tuple()) -> ok.
%% @doc 向数据库中更新数据
update(Rec) ->
	amnesia:update(?AMNESIA_DB_PID, Rec).

-spec delete(Rec::tuple()) -> ok.
%% @doc 从数据库中删除数据
delete(Rec) ->
	ok = amnesia:delete(?AMNESIA_DB_PID, Rec).


