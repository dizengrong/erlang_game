%% @doc 通用缓存系统
%% 实现说明：
%% 这里对每个数据库表使用了三张ets表
%% 		1.record数据表：用来存在具体数据的，
%% 		  不过由于mysql和ets关键字问题，我做了一个双休转化的：
%%		  rec_transform_cache_to_db:缓存ets中的数据转化到mysql
%% 		  rec_transform_db_to_cache:mysql数据转化到缓存ets中
%% 		2.index索引表：因为我们的数据大都是以玩家id为建立的，会有一对多的情况
%%		  因此需要对record中的某个字段做索引(#cache_config.index_field)
%% 		  索引表的数据为{Index, Keys}，其中Keys为#cache_config.key_fields中的字段的值的列表
%%		3.dirty脏数据表：被更新过的数据都会变为脏数据，然后对他们进行记录，以便进行差量更新
%%		　其数据为#cache_config.key_fields中的字段的值的列表

-module (gen_cache).
-include ("gen_cache.hrl").
-include ("log.hrl").

-export([start_link/2]).
-export([lookup/3, lookup_element/4, insert/3, update_element/4, update/3, delete/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([save_dirty_datas_to_db/1]).

-define(SERVER, ?MODULE).
-define(SAVE_INTERVAL_1, 20*60*1000). 	%% 保存缓存数据到数据库的间隔1(ms)
-define(SAVE_INTERVAL_2, 40*60*1000). 	%% 保存缓存数据到数据库的间隔2(ms)

start_link(RegisterName, TableNames) ->
	%% TODO:使用global来注册进程，是为了获得节点透明性的好处
	%% 但是global名称在分布式环境下的同步问题可能会存在通信量问题
	%% 因此这里以后可以改成local方式来注册进程，然后再封装一套RPC方法来获取数据
	gen_server:start_link({global, RegisterName}, ?MODULE, {TableNames}, []).

-spec lookup(CacheName::any(), TableName::atom(), Key::term()) -> list().
%% @doc lookup支持两种查询：
%% 1.Key只是索引字段值的查询
%% 2.Key是一个list，为多关键字表的所有关键字按照#cache_config.key_fields中给定的顺序的组合
lookup(CacheName, TableName, Key) ->
	gen_server:call(CacheName, {lookup, TableName, Key}).

-spec lookup_element(CacheName::any(), TableName::atom(), Key::term(), Pos::integer()) -> term().
%% @doc 和ets:lookup_element/3方法一样
lookup_element(CacheName, TableName, Key, Pos) ->
	gen_server:call(CacheName, {lookup_element, TableName, Key, Pos}).

-spec insert(CacheName::any(), TableName::atom(), Rec::list() | tuple()) -> {ok, list() | tuple()}.
%% @doc 同步数据插入，会立马插入到数据库中并更新缓存
%% @param Rec: 为一个record或者record的列表
insert(CacheName, TableName, Rec) ->
	gen_server:call(CacheName, {insert, TableName, Rec}).

-spec update(CacheName::any(), TableName::atom(), Rec::list() | tuple()) -> any().
%% @doc 更新数据
%% @param Rec: 为一个record或者record的列表
update(CacheName, TableName, Rec) ->
	gen_server:cast(CacheName, {update, TableName, Rec}).

-spec update_element(CacheName::any(), 
					 TableName::atom(), 
					 Key::term(), 
					 ElementSpec::[{Pos::integer(), Value::term()}]) -> any().
%% @doc 更新record数据中的某些字段，和ets:update_element/3类似
%% 参数Key为数据记录的关键字(单关键字表就是一个值，多关键字表则为它们的元组)
update_element(CacheName, TableName, Key, ElementSpec) ->
	gen_server:cast(CacheName, {update_element, TableName, Key, ElementSpec}).

%% @doc 删除record数据，同时也会删除数据库中的记录
%% 参数Key同lookup/3的Key参数，所以也支持2中删除方式的
delete(CacheName, TableName, Key) ->
	gen_server:cast(CacheName, {delete, TableName, Key}).


-spec save_cache_to_db(CachePid::pid(), DelayTime::integer()) -> any().
%% @doc 将缓存中的脏数据延迟DelayTime毫秒写入数据库
save_cache_to_db(CachePid, DelayTime) ->
	timer:send_after(DelayTime, CachePid, {save_cache_to_db}).

%% @private
init({TableNames}) ->
    erlang:process_flag(trap_exit, true),
	init_tables(TableNames),
	set_cached_tables(TableNames),
	Time = util_random:random_between(?SAVE_INTERVAL_1, ?SAVE_INTERVAL_2),
	save_cache_to_db(self(), Time),
	{ok, undefined}.

init_tables([]) -> ok;
init_tables([TableName | Rest]) ->
	Tab1   = set_rec_tab(TableName),
	Tab2   = set_dirty_tab(TableName),
	Tab3   = set_index_tab(TableName),
	KeyPos = get_rec_tab_key_pos(TableName),
	ets:new(Tab1, [named_table, public, set, {keypos,KeyPos}]), %% 表数据ets
	ets:new(Tab2, [named_table, public, set, {keypos,1}]), 		%% 脏数据索引表
	ets:new(Tab3, [named_table, public, set, {keypos,1}]), 		%% 索引字段的索引表
	init_tables(Rest).

get_rec_tab_key_pos(TableName) -> 
	Conf   = cache_config:conf(TableName),
	Conf#cache_config.index_field.

%% @private
handle_call({lookup, TableName, Key}, _From, State) ->
	Reply = lookup2(TableName, Key),
	{reply, Reply, State};
handle_call({lookup_element, TableName, Key, Pos}, _From, State) ->
	case lookup2(TableName, Key) of
		[] 	  -> Reply = badarg;
		[Rec] -> Reply = element(Pos, Rec);
		Recs  -> Reply = [element(Pos, R) || R <- Recs]
	end,
	{reply, Reply, State};
handle_call({insert, TableName, Rec}, _From, State) ->
	Reply = insert2(TableName, Rec),
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

%% @private
handle_cast({update_element, TableName, Key, ElementSpec}, State) ->
	update_element2(TableName, Key, ElementSpec),
	{noreply, State};
handle_cast({update, TableName, Rec}, State) ->
	update2(TableName, Rec),
	{noreply, State};
handle_cast({delete, TableName, Key}, State) ->
	delete2(TableName, Key),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info({save_cache_to_db}, State) ->
	[async_save_dirty_datas_to_db(TableName) || TableName <- get_cached_tables()],
	Time = util_random:random_between(?SAVE_INTERVAL_1, ?SAVE_INTERVAL_2),
	save_cache_to_db(self(), Time),
	{noreply, State};
handle_info({'EXIT', _PID, Reason}, State) ->
    ?ERROR("~p recieve EXIT message, reason: ~p, state: ~p", [?MODULE, Reason, State]),
    {stop, normal, State}.

%% @private
terminate(Reason, State) ->
	[save_dirty_datas_to_db(TableName) || TableName <- get_cached_tables()],
    ?ERROR("~p terminate, reason: ~p, state: ~p", [?MODULE, Reason, State]),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

async_save_dirty_datas_to_db(TableName) ->
	save_dirty_datas_to_db(TableName).
	% erlang:spawn(?MODULE, save_dirty_datas_to_db, [TableName]).

save_dirty_datas_to_db(TableName) ->
	EtsDirty  = get_dirty_tab(TableName),
	DirtyList = ets:tab2list(EtsDirty),
	ets:delete_all_objects(EtsDirty),
	save_dirty_datas_to_db2(TableName, DirtyList).

save_dirty_datas_to_db2(_TableName, []) -> ok;
save_dirty_datas_to_db2(TableName, [KeyVals | Rest]) ->
	try
		[DirtyRec | _]  = lookup_from_cache(get_rec_tab(TableName), KeyVals),
		DirtyRec2 = rec_transform_cache_to_db(TableName, DirtyRec),
		db:update(DirtyRec2)
	catch
		Type:Reason -> ?PRINT_STACKTRACE(save_dirty_datas_to_db2, Type, Reason)
	end,
	save_dirty_datas_to_db2(TableName, Rest).

%% @doc 将cache中的record转为db中的record
rec_transform_cache_to_db(TableName, Rec) ->
	Conf = cache_config:conf(TableName),
	case cache_config:is_multi_keys(Conf) of
		false -> Rec;
		true  -> 
			IndexField = Conf#cache_config.index_field,
			KeyVals    = element(IndexField, Rec),
			IndexKey   = get_index_key(IndexField, Conf#cache_config.key_fields, KeyVals),
			Rec2       = setelement(IndexField, Rec, IndexKey),
			Rec2
	end.

%% @doc 将db中的record转为cache中的record
rec_transform_db_to_cache(TableName, Rec) ->
	Conf = cache_config:conf(TableName),
	case cache_config:is_multi_keys(Conf) of
		false -> Rec;
		true  ->
			KeyVals = index_keys(Rec, Conf#cache_config.key_fields, []),
			Rec2    = setelement(Conf#cache_config.index_field, Rec, KeyVals),
			Rec2
	end.

get_index_key(IndexField, [], []) -> erlang:throw({index_field_not_in_key_fields, IndexField});
get_index_key(IndexField, [IndexField | _Rest1], [KeyVal | _Rest2]) -> KeyVal;
get_index_key(IndexField, [_ | Rest1], [_ | Rest2]) ->  get_index_key(IndexField, Rest1, Rest2).

lookup2(TableName, Key) ->
	Conf   = cache_config:conf(TableName),
	EtsRec = get_rec_tab(TableName),
	case cache_config:is_multi_keys(Conf) of
		false ->
			case lookup_from_cache(EtsRec, Key) of
				[] 	 -> lookup_from_db(TableName, Key);
				Recs -> Recs
			end;
		true ->
			EtsIndex = get_index_tab(TableName),
			case is_list(Key) of
				true -> 
					case lookup_from_cache(EtsIndex, lists:nth(1, Key)) of
						[] -> lookup_from_db(TableName, Key);
						_  -> %% 直接根据key值取数据
							lookup_from_cache(EtsRec, Key)
					end;
				false ->
					case lookup_from_cache(EtsIndex, Key) of
						[] 	   -> lookup_from_db(TableName, Key);
						Indexs -> lists:flatten([lookup_from_cache(EtsRec, Index) || Index <- Indexs])
					end
			end
	end.

lookup_from_cache(Ets, Key) -> ets:lookup(Ets, Key).

lookup_from_db(TableName, Key) ->
	Conf = cache_config:conf(TableName),
	KeyFields = Conf#cache_config.key_fields,
	case is_list(Key) of
		true  -> 
			FieldNames = [record_fields:get_field_name(TableName, I) || I <- KeyFields],
			PredSpec   = predicate_spec(FieldNames),
			ValueList  = tuple_to_list(Key);
		false -> 
			IndexField = Conf#cache_config.index_field,
			FieldNames = record_fields:get_field_name(TableName, IndexField),
			PredSpec   = predicate_spec([FieldNames]),
			ValueList  = [Key]
	end,
	Recs = db:fetch(TableName, {PredSpec, ValueList}),
	[add_to_cache(TableName, Rec) || Rec <- Recs],
	Recs.

-spec add_to_cache(TableName::atom(), Rec::tuple()) -> any().
%% @doc 增加新的数据到cache里
add_to_cache(TableName, Rec) ->
	Conf          = cache_config:conf(TableName),
	Rec2          = rec_transform_db_to_cache(TableName, Rec),
	EtsIndex      = get_index_tab(TableName),
	IndexKey      = element(Conf#cache_config.index_field, Rec),
	KeyVals1      = element(Conf#cache_config.index_field, Rec2),
	case lookup_from_cache(EtsIndex, IndexKey) of
		[] -> KeyVals3 = KeyVals1;
		[{_, KeyVals2}] -> KeyVals3 = KeyVals1 ++ KeyVals2
	end,
	ets:insert(EtsIndex, {IndexKey, KeyVals3}),
	ets:insert(get_rec_tab(TableName), Rec2).

insert2(TableName, Recs) when is_list(Recs) ->
	Recs2 = [begin {ok, Rec2} = insert2(TableName, Rec), Rec2 end || Rec <- Recs],
	{ok, Recs2};
insert2(TableName, Rec) ->
	{ok, Rec2} = db:add_new(Rec),
	add_to_cache(TableName, Rec2),
	{ok, Rec2}.

update2(TableName, Recs) when is_list(Recs) ->
	[update2(TableName, Rec) || Rec <- Recs];
update2(TableName, Rec) ->
	update_to_cache(TableName, Rec),
	ok.

-spec update_to_cache(TableName::atom(), Rec::tuple()) -> any().
%% @doc更新数据到cache里，同时更新数据脏标志
update_to_cache(TableName, Rec) ->
	ets:insert(get_rec_tab(TableName), Rec),
	Conf      = cache_config:conf(TableName),
	IndexKeys = index_keys(Rec, Conf#cache_config.key_fields, []),
	ets:insert(get_dirty_tab(TableName), {IndexKeys}),
	ok.

update_element2(TableName, IndexKeys, ElementSpec) ->
	ets:update_element(get_rec_tab(TableName), IndexKeys, ElementSpec),
	ets:insert(get_dirty_tab(TableName), {IndexKeys}),
	ok.

delete2(TableName, Key) ->
	Recs     = lookup2(TableName, Key),
	Conf     = cache_config:conf(TableName),
	EtsRec   = get_rec_tab(TableName),
	EtsDirty = get_dirty_tab(TableName),
	EtsIndex = get_index_tab(TableName),
	KeyPos   = get_rec_tab_key_pos(TableName),
	[begin IndexKeys = index_keys(Rec, Conf#cache_config.key_fields, []),
		   %% 删除索引表中的数据
		   case cache_config:is_multi_keys(Conf) of
		   	   false -> ok;
		   	   true  ->
				   IndexKey = element(Conf#cache_config.index_field, Rec),
		   	   	   case lookup_from_cache(EtsIndex, IndexKey) of
		   	   	   		[] -> ok;
		   	   	   		[{_, KeyVals}] -> 
		   	   	   			KeyVals2 = lists:delete(IndexKeys, KeyVals),
		   	   	   			ets:insert(EtsIndex, {IndexKey, KeyVals2})
		   	   	   end
		   end,		   
		   ets:delete(EtsDirty, IndexKeys),
		   ets:delete(EtsRec, element(KeyPos, Rec)),
		   %% 最后从数据库中删除
		   db:delete(Rec)
	 end || Rec <- Recs],
	ok.



index_keys(_Rec, [], Values) -> lists:reverse(Values);
index_keys(Rec, [Index | Rest], Values) ->
	index_keys(Rec, Rest, [element(Index, Rec) | Values]).

predicate_spec([KeyField | Rest]) ->
	Spec = atom_to_list(KeyField) ++ " = $1",
	predicate_spec(Rest, Spec, 2).
predicate_spec([], Spec, _Count) -> Spec;
predicate_spec([KeyField | Rest], Spec, Count) ->
	Spec2 = Spec ++ ", " ++ atom_to_list(KeyField) ++ " = $" ++ integer_to_list(Count),
	predicate_spec(Rest, Spec2, Count + 1).

set_rec_tab(TableName) -> 
	Tab = list_to_atom("ets_rec_" ++ atom_to_list(TableName)),
	erlang:put({ets_rec, TableName}, Tab),
	Tab.
get_rec_tab(TableName) -> erlang:get({ets_rec, TableName}).

set_dirty_tab(TableName) ->
	Tab = list_to_atom("ets_dirty_" ++ atom_to_list(TableName)), 
	erlang:put({ets_dirty, TableName}, Tab),
	Tab.
get_dirty_tab(TableName) -> erlang:get({ets_dirty, TableName}).

set_index_tab(TableName) ->
	Tab = list_to_atom("ets_index_" ++ atom_to_list(TableName)), 
	erlang:put({ets_index, TableName}, Tab),
	Tab.
get_index_tab(TableName) -> erlang:get({ets_index, TableName}).

set_cached_tables(TableNames) -> erlang:put(cached_tables, TableNames).
get_cached_tables() -> erlang:get(cached_tables).
