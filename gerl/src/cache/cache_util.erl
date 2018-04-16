%% @author dzR <dizengrong@gmail.com>
%% @doc gen_cache相关的一些方法

-module(cache_util).
-export([get_cache_name/1]).

%% @doc 根据Tab表名来获取其对应的缓存进程名
get_cache_name(Tab) ->
	{global, cache_config:cache_name(db_module:tab2db_mod(Tab))}.

% -include("gen_cache.hrl").
% -include("log.hrl").

% -export([select/3, insert/2, update/2, select_all/2, delete/2,
% 		 get_key_index_ets/1,
% 		 get_register_name/1, get_record_size/1,
% 		 get_update_index_ets/1,
% 		 update_to_db/2, save_all_right_now/0,
% 		 start_update_to_db/1,
% 		 remove_all_cache_data/1]).
% %% other useful utility
% -export([string_to_term/1, bitstring_to_term/1,
%          term_to_string/1, term_to_bitstring/1]).

% %%
% %% API Functions
% %%
% save_all_right_now() ->
% 	AllGenCache = [cache_util:get_register_name(Rec) || Rec <- cache_config:tables()],
% 	[cache_util:start_update_to_db(CacheRef) || CacheRef <- AllGenCache].



% start_update_to_db(CacheRef) ->
% 	gen_server:cast(CacheRef, start_update_to_db).

% update_to_db(CacheRef, UpdateList) ->
% 	gen_server:cast(CacheRef, {update_to_db, UpdateList}).

% get_key_index_ets(RecEts) ->
% 	%% TODO:我这里想用list_to_existing_atom
% 	%% (因为list_to_atom会创建一个新的atom，并且atom不会参与垃圾回收的)，
% 	%% 但若这个atom之前没有被创建会出错的
% 	%% 一个解决的办法是在一个全局的开始地方先创建这些atom
% 	%% 另一个办法是使用try cath，出错时就使用list_to_atom来创建
% 	list_to_atom(atom_to_list(RecEts) ++ "_index").

% get_update_index_ets(Record) ->
% 	list_to_atom(atom_to_list(Record) ++ "_update_index").

% %% gen_cache进程的注册名
% get_register_name(Record) -> Record.

% %% 获取gen_cache对应的ets表中当前记录的数量
% get_record_size(Record) ->
% 	MapRec = cache_config:map(Record),
% 	if
% 		MapRec == none ->
% 			undefined;
% 		true ->
% 			ets:info(MapRec#cache_config.ets_tab, size)
% 	end.

% select_all(Record, Mapper) ->
% 	Sql = cache_mapper:gen_select_all(Record),
% 	?INFO( "select_all sql is: ~s", [Sql]),
% 	case db_sql:get_all(Sql) of
% 		[] -> 
% 			?INFO( "There is no data in database"),
% 			[];
% 		Datas ->
% 			RecordList = fill_record(Record, Mapper, Datas),
% 			RecordList			
% 	end.

% select(Record, Mapper, Id) ->
% 	Sql = cache_mapper:gen_select(Record, Id),
% 	?INFO( "select sql is: ~s", [Sql]),
% 	case db_sql:get_all(Sql) of
% 		[] -> 
% 			?INFO( "There is no data in database with id: ~w", [Id]),
% 			[];
% 		Datas ->
% 			RecordList = fill_record(Record, Mapper, Datas),
% 			RecordList			
% 	end.

% insert(Record, RecordData) ->
% 	Sql = cache_mapper:gen_insert(Record, RecordData),
% 	?INFO( "insert sql is: ~s", [Sql]),
% 	db_sql:execute(Sql).

% update(Record, RecordData) ->
% 	Sql = cache_mapper:gen_update(Record, RecordData),
% 	?INFO( "update sql is: ~s", [Sql]),
% 	db_sql:execute(Sql).

% delete(Record, RecordData) ->
% 	Sql = cache_mapper:gen_delete(Record, RecordData),
% 	?INFO( "delete sql is: ~s", [Sql]),
% 	db_sql:execute(Sql).
% %%
% %% Local Functions
% %%

% fill_record(_Record, _Map, []) -> [];
% fill_record(RecordName, Map, [Datas | DatasRest]) ->
% 	RecordData = case Map#cache_config.key_classic of
% 		0 ->
% 			[_ | TypesSpec] = erlang:tuple_to_list(Map#cache_config.fields_spec),
% 			Datas1 = format_record_field(TypesSpec, Datas, []),
% 			erlang:list_to_tuple([RecordName | Datas1]);
% 		1 ->
% 			[_, KeySpec | TypesSpec] = erlang:tuple_to_list(Map#cache_config.fields_spec),
% 			[Key1 | Rest] = Datas,
% 			Datas1 = format_record_field(TypesSpec, Rest, []),
% 			KeySpec1 = cache_mapper:make_key_spec(KeySpec),
% 			Key = format_record_field(KeySpec1, [Key1], []),
% 			erlang:list_to_tuple([RecordName | lists:append(Key, Datas1)]);
% 		2 -> %% TODO: 如果key可以为字符串，则要为key做转化
% 			[_, KeySpec | TypesSpec] = erlang:tuple_to_list(Map#cache_config.fields_spec),
% 			[Key1, Key2 | Rest] = Datas,
% 			% [Key1Spec, Key2Spec] = cache_mapper:make_key_spec(KeySpec),
% 			KeySpec1 = cache_mapper:make_key_spec(KeySpec),
% 			Key = format_record_field(KeySpec1, [Key1, Key2], []),
% 			Datas1 = format_record_field(TypesSpec, Rest, []),
% 			% Key = {format_field(Key1, Key1Spec), format_field(Key2, Key2Spec)}
% 			erlang:list_to_tuple([RecordName, erlang:list_to_tuple(Key) | Datas1]);
% 		3 -> 
% 			[_, KeySpec | TypesSpec] = erlang:tuple_to_list(Map#cache_config.fields_spec),
% 			KeySpec1 = cache_mapper:make_key_spec(KeySpec),
% 			[Key1, Key2, Key3 | Rest] = Datas,
% 			Key = format_record_field(KeySpec1, [Key1, Key2, Key3], []),
% 			Datas1 = format_record_field(TypesSpec, Rest, []),
% 			erlang:list_to_tuple([RecordName, erlang:list_to_tuple(Key) | Datas1])
% 	end,
% 	[RecordData | fill_record(RecordName, Map, DatasRest)].

% %% 如果没有对于的字段值了，则根据该record对应的type_spec来给默认值
% format_record_field([], [], FormatedFieldDatas) -> lists:reverse(FormatedFieldDatas);
% format_record_field([TypeSpec | Rest1], [], FormatedFieldDatas) -> 
% 	FormatedField = default_field_value(TypeSpec),
% 	format_record_field(Rest1, [], [FormatedField | FormatedFieldDatas]);
% format_record_field([TypeSpec | Rest1], [FieldData | Rest2], FormatedFieldDatas) ->
% 	FormatedField = format_field(TypeSpec, FieldData),
% 	format_record_field(Rest1, Rest2, [FormatedField | FormatedFieldDatas]).
	
% format_field(TypeSpec, FieldData) ->
% 	case TypeSpec of
% 		{string} -> FormatedField = binary_to_list(FieldData);
% 		{term} -> 	FormatedField = bitstring_to_term(FieldData);
% 		{integer} ->FormatedField = FieldData
% 	end,
% 	FormatedField.

% default_field_value(TypeSpec) ->
% 	case TypeSpec of
% 		{string} -> 	FormatedField = "";
% 		{term} -> 		FormatedField = [];
% 		{integer} -> 	FormatedField = 0
% 	end,
% 	FormatedField.

% %% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
% bitstring_to_term(undefined) -> undefined;
% bitstring_to_term(BitString) ->
%     string_to_term(binary_to_list(BitString)).

% %% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
% term_to_bitstring(Term) ->
%     erlang:list_to_bitstring(io_lib:format("~w", [Term])).

% %% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
% string_to_term(String) ->
%     case erl_scan:string(String++".") of
%         {ok, Tokens, _} ->
%             case erl_parse:parse_term(Tokens) of
%                 {ok, Term} -> Term;
%                 _Err -> undefined
%             end;
%         _Error ->
%             undefined
%     end.

% %% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
% term_to_string(Term) ->
% 	lists:flatten(io_lib:format("~w", [Term])).

% remove_all_cache_data(Id)->
% 	AllGenCache = [cache_util:get_register_name(Rec) || Rec <- cache_config:tables()],
% 	[gen_cache:remove_cache_data(CacheRef, Id) || CacheRef <- AllGenCache].
