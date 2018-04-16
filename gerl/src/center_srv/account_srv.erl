%% @author dzR <dizengrong@gmail.com>
%% @doc 全局的账号服务进程，管理账号的注册和查询

-module (account_srv).
-include("db_table_account.hrl").
-include("db_table_role.hrl").
-include("db_table_magic.hrl").
-include("db_table_rune.hrl").
-include("log.hrl").

-export([start_link/0]).
-export([is_account_exist/1, get_role_id_by_account/1, register_account/3]).
-export([get_role_id_by_name/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, {global, ?MODULE}).

-define (REGISTER_ACCOUNT_EXIST, 	1). 	%% 账号已存在
-define (REGISTER_ROLE_NAME_EXIST, 	2). 	%% 角色名已存在
-define (REGISTER_SYSTEM_ERROR, 	3). 	%% 服务器发生错误了

-define(MAX_ACCOUNT_PER_SERVER, 1000000).	%% 每个服务器最大的账号容量

%% 账号名与账号id的索引表
-record (account_id_map, {
	account = "", 
	role_id = 0
}).

%% 角色名与账号id的索引表
-record (role_name_id_map, {
	role_name = "", 
	role_id   = 0
}).

start_link() ->
	gen_server:start_link(?SERVER, ?MODULE, {}, []).

-spec is_account_exist(AccountName::string()) -> boolean().
%% @doc 账号是否已存在了（即该账号是否已注册了）
is_account_exist(AccountName) ->
	gen_server:call(?SERVER, {is_account_exist, AccountName}).

-spec get_role_id_by_account(AccountName::string()) -> non_neg_integer().
%% @doc 根据账号名获取role_id，不存在账号的话返回0
get_role_id_by_account(AccountName) ->
	gen_server:call(?SERVER, {get_role_id_by_account, AccountName}).


-spec get_role_id_by_name(RoleName::string()) -> non_neg_integer().
%% @doc 根据玩家名获取role_id，不存在的话返回0
get_role_id_by_name(RoleName) ->
	gen_server:call(?SERVER, {get_role_id_by_name, RoleName}).

-spec register_account(AccountName::string(), 
					   RoleName::string(),
					   HeroId::integer()) -> {ok, NewRoleId::integer()} | {error, integer()}.
%% @doc 根据账号名获取role_id，不存在账号的话返回0
register_account(AccountName, RoleName, HeroId) ->
	gen_server:call(?SERVER, {register_account, AccountName, RoleName, HeroId}).


%% @private
init({}) ->
	Accounts = db:get_all_accounts(),
	init_account_id_index(Accounts),
	init_role_name_index(Accounts),
	init_max_role_id(),
	{ok, undefined}.

init_account_id_index(Accounts) ->
	ets:new(ets_account_id_index, [public,named_table,set,{keypos, #account_id_map.account}]),
	Fun = fun(Rec) ->
		Rec2 = #account_id_map{account = Rec#tab_account.account, role_id = Rec#tab_account.role_id},
		ets:insert(ets_account_id_index, Rec2)
	end,
	[Fun(R) || R <- Accounts].

init_role_name_index(Accounts) ->
	ets:new(ets_role_name_index, [public,named_table,set,{keypos, #role_name_id_map.role_name}]),
	Fun = fun(Rec) ->
		Rec2 = #role_name_id_map{role_name = Rec#tab_account.role_name, role_id = Rec#tab_account.role_id},
		ets:insert(ets_role_name_index, Rec2)
	end,
	[Fun(R) || R <- Accounts].

init_max_role_id() ->
	ets:new(ets_max_role_id, [public,named_table,set]),
	case db:get_max_role_id() of
		0	-> Max = (gerl_setting:get(server_index) - 1) * ?MAX_ACCOUNT_PER_SERVER;
		Max -> ok
	end,
	ets:insert(ets_max_role_id, {max_role_id, Max}).

%% @private
handle_call({is_account_exist, AccountName}, _From, State) ->
	{reply, is_account_exist2(AccountName), State};
handle_call({register_account, AccountName, RoleName, HeroId}, _From, State) ->
	Reply = case is_account_exist2(AccountName) of
		true -> {error, ?REGISTER_ACCOUNT_EXIST};
		false ->
			case is_role_name_exist2(RoleName) of
				true  -> {error, ?REGISTER_ROLE_NAME_EXIST};
				false ->
					create_new_role(AccountName, RoleName, HeroId)
			end
	end,
	{reply, Reply, State};

handle_call({get_role_id_by_account, AccountName}, _From, State) ->
	Reply = case ets:lookup(ets_account_id_index, AccountName) of
		[] 	  -> 0;
		[Rec] -> Rec#account_id_map.role_id
	end,
	{reply, Reply, State};

handle_call({get_role_id_by_name, RoleName}, _From, State) ->
	Reply = case ets:lookup(ets_role_name_index, RoleName) of
		[] 	  -> 0;
		[Rec] -> Rec#role_name_id_map.role_id
	end,
	{reply, Reply, State};


handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

is_account_exist2(AccountName) ->
	(ets:lookup(ets_account_id_index, AccountName) /= []).

is_role_name_exist2(RoleName) ->
	(ets:lookup(ets_role_name_index, RoleName) /= []).

create_new_role(AccountName, RoleName, HeroId) ->
	try
		[{_, CurMaxId}] = ets:lookup(ets_max_role_id, max_role_id),
		NewRoleId     = CurMaxId + 1,
		AccountRec = #tab_account{
			account   = AccountName, 
			role_id   = NewRoleId, 
			role_name = RoleName
		},
		FightAttrRec = #tab_fight_attr{
			role_id    = NewRoleId,
			hero_id    = HeroId,
			move_speed = cfg_init_datas:get(move_speed)
		},
		TroopsRecs = init_troops(NewRoleId, HeroId),
		RoleDataRec = #tab_role_data{
			role_id = NewRoleId 
		},
		MagicDataRec = #tab_magic_data{
			role_id = NewRoleId
		},
		RuneRec = #tab_rune{
			role_id = NewRoleId
		},
		ChatRec = #tab_role_chat{
			role_id = NewRoleId
		},
		MapId      = cfg_init_datas:get(newer_mapid),
		{X, Y}     = cfg_map:born_pos(MapId),
		RolePosRec = role:make_tab_pos_rec(NewRoleId, MapId, X, Y),
		%% TODO:数据库插入式同步的，可以做插入数据是否都ok的try/catch，并做回滚
		db_account:insert_account_rec(AccountRec),
		db_role:insert_fight_attr_rec(FightAttrRec),
		db_role:insert_troops_rec(TroopsRecs),
		db_role:insert_role_pos_rec(RolePosRec),
		db_role:insert_role_data_rec(RoleDataRec),
		db_role:insert_role_chat_rec(ChatRec),
		db_magic:insert_magic_data_rec(MagicDataRec),
		db_rune:insert_rune_rec(RuneRec),

		Rec1 = #account_id_map{
			account = AccountName, 
			role_id = NewRoleId
		},
		Rec2 = #role_name_id_map{
			role_name = RoleName,
			role_id   = NewRoleId
		},
		ets:insert(ets_account_id_index, Rec1),
		ets:insert(ets_role_name_index, Rec2),
		{ok, NewRoleId}
	catch
		Type:Error ->
			?EMERGENCY("Register account failed, type:~p, error:~p, stack:~p", 
					   [Type, Error, erlang:get_stacktrace()]),
			{error, ?REGISTER_SYSTEM_ERROR}
	end.

init_troops(NewRoleId, HeroId) ->
	[#tab_troops{
		role_id       = NewRoleId,
		troops_id     = TroopsId,
		troops_amount = TroopsAmount,
		place         = TroopsPlace,
		level         = TroopsLevel
	 } || {TroopsId, TroopsAmount, TroopsPlace, TroopsLevel} <- cfg_hero:get_troops(HeroId)].
