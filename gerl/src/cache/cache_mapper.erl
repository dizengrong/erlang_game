%% Author: dzr
%% Created: 2012-2-7
%% Description: TODO: Add description to mapper
-module(cache_mapper).

% %%
% %% Include files
% %%
% -include("gen_cache.hrl").
% -include("log.hrl").
% %%
% %% Exported Functions
% %%
% -export([gen_select/2, gen_insert/2, gen_update/2, gen_delete/2, gen_select_all/1]).

% -export([make_key_spec/1]).
% %% -export([make_set/4]).
% % -compile(export_all).
% %%
% %% API Functions
% %%
% %% 生成对于record名称的sql select语句
% gen_select(RecordName, KeyValue) ->
% 	Map = cache_config:map(RecordName),
% 	make_select(Map, KeyValue, false).

% gen_select_all(RecordName) ->
% 	Map = cache_config:map(RecordName),
% 	make_select(Map, none, true).

% gen_insert(RecordName, RecordData) ->
% 	Map = cache_config:map(RecordName),
% 	make_insert(Map, RecordData).

% gen_update(RecordName, RecordData) ->
% 	Map = cache_config:map(RecordName),
% 	make_update(Map, RecordData).

% gen_delete(RecordName, RecordData) ->
% 	Map = cache_config:map(RecordName),
% 	make_delete(Map, RecordData).
% %%
% %% Local Functions
% %%
% make_delete(Map, RecordData) ->
% 	case Map#cache_config.key_classic of
% 		0 ->
% 			Fields = lists:sublist(Map#cache_config.fields, length(Map#cache_config.fields) - length(Map#cache_config.ignored_fields)),
% 			[_ | FieldValues] = erlang:tuple_to_list(RecordData),
% 			Values = lists:sublist(FieldValues, length(FieldValues) - length(Map#cache_config.ignored_fields)),
% 			[_ | FieldSpecs] = erlang:tuple_to_list(Map#cache_config.fields_spec),
% 			FieldSpecs1 = lists:sublist(FieldSpecs, length(FieldSpecs) - length(Map#cache_config.ignored_fields)),
% 			WhereCond = make_where(Fields, Values, FieldSpecs1);
% 		_ ->
% 			[_, Key | _FieldValues] = erlang:tuple_to_list(RecordData),
% 			case Map#cache_config.key_classic of
% 				1 -> KeyValues = [Key];
% 				_ -> KeyValues = erlang:tuple_to_list(Key)
% 			end,
% 			[_ , KeySpec | _FieldSpecs] = erlang:tuple_to_list(Map#cache_config.fields_spec),
% 			KeySpec1 = make_key_spec(KeySpec),
% 			WhereCond = make_where(Map#cache_config.key_fields, KeyValues, KeySpec1)
% 	end,
% 	lists:concat(["delete from ", Map#cache_config.sql_tab, " where ", WhereCond]).

% make_select(Map, KeyValue, IsSelectFromAll) ->
% 	case Map#cache_config.key_classic of
% 		0 ->
% 			Fields1 = make_fields(Map#cache_config.fields, "", Map#cache_config.ignored_fields);
% 		_ ->
% 			Fields1 = make_fields(Map)
% 	end,
% 	case IsSelectFromAll of
% 		true ->  WhereCond = "1 = 1";
% 		false -> 
% 			[_ , KeySpec | _FieldSpecs] = erlang:tuple_to_list(Map#cache_config.fields_spec),
% 			KeySpec1 = make_key_spec(KeySpec),
% 			WhereCond = make_where(Map#cache_config.key_fields, [KeyValue], KeySpec1)
% 	end,
% 	lists:concat(["select ", Fields1, " from ", Map#cache_config.sql_tab, " where ", WhereCond]).

% make_fields(Map) ->
% 	case Map#cache_config.key_classic of
% 		0 -> Rest = Map#cache_config.fields;
% 		_ -> [_Field | Rest] = Map#cache_config.fields		
% 	end,
% 	S = make_key_fields(Map#cache_config.key_classic, Map#cache_config.key_fields),
% 	make_fields(Rest, S, Map#cache_config.ignored_fields).

% make_fields([], S, _IgnoredFields) -> S;
% make_fields([F | Rest], S, IgnoredFields) ->
% 	case lists:member(F, IgnoredFields) of
% 		true ->   S1 = S;
% 		_false -> 
% 			case S of 
% 				"" ->
% 					S1 = erlang:atom_to_list(F);
% 				_ ->
% 					S1 = S ++ ", " ++ erlang:atom_to_list(F)
% 			end
% 	end,
% 	make_fields(Rest, S1, IgnoredFields).

% make_key_fields(KeyClassic, Keys) ->
% 	case KeyClassic of
% 		0 ->
% 			"";
% 		1 ->
% 			[Key1 | _] = Keys,
% 			erlang:atom_to_list(Key1);
% 		2 ->
% 			[Key1, Key2 | _] = Keys,
% 			erlang:atom_to_list(Key1) ++ ", " ++ erlang:atom_to_list(Key2);
% 		3 -> 
% 			[Key1, Key2, Key3 | _] = Keys,
% 			erlang:atom_to_list(Key1) ++ ", " ++ 
% 				erlang:atom_to_list(Key2) ++ ", " ++ 
% 				erlang:atom_to_list(Key3)
% 	end.

% make_where([KeyField | Rest1], [KeyValue | Rest2], [FieldSpec | Rest3]) ->
% 	S1 = lists:concat([KeyField, " = '", field_value_format(KeyValue, FieldSpec), "'"]),
% 	make_where(Rest1, Rest2, S1, Rest3).

% make_where(_, [], S, _) -> S;
% make_where([KeyField | Rest1], [KeyValue | Rest2], S, [FieldSpec | Rest3]) ->
% 	S1 = lists:concat([S, " and ", KeyField, " = '", field_value_format(KeyValue, FieldSpec), "'"]),
% 	make_where(Rest1, Rest2, S1, Rest3).

% make_insert(Map, RecordData) ->
% 	Fields = make_fields(Map),
% 	Values = make_values(Map, RecordData),
% 	lists:concat(["INSERT INTO ", Map#cache_config.sql_tab, "(", Fields, ") VALUES (", Values, ")"]).
	
% make_values(Map, RecordData) ->
% 	[_, Key | FieldValues] = erlang:tuple_to_list(RecordData),
% 	[_, KeySpec | FieldSpecs] = erlang:tuple_to_list(Map#cache_config.fields_spec),
% 	[_KeyField | FieldList] = Map#cache_config.fields,
% 	S1 = make_key_values(Map, Key, KeySpec),
% 	S2 = make_field_values(FieldValues, FieldList, FieldSpecs, Map#cache_config.ignored_fields),
% 	case S2 of
% 		[] -> S1;
% 		_ -> S1 ++ ", " ++ S2
% 	end.

% make_key_values(Map, Key, KeySpec) ->
% 	case Map#cache_config.key_classic of
% 		0 ->
% 			"'" ++ field_value_format(Key, KeySpec) ++ "'";
% 		1 -> 
% 			"'" ++ field_value_format(Key, KeySpec) ++ "'";
% 		2 ->
% 			{Key1, Key2} = Key,
% 			{Key1Spec, Key2Spec} = KeySpec,
% 			"'" ++ field_value_format(Key1, Key1Spec) ++ "', '" ++ field_value_format(Key2, Key2Spec) ++ "'";
% 		3 ->
% 			{Key1, Key2, Key3} = Key,
% 			{Key1Spec, Key2Spec, Key3Spec} = KeySpec,
% 			"'" ++ field_value_format(Key1, Key1Spec) ++ "', '" ++ 
% 			field_value_format(Key2, Key2Spec) ++ "', '" ++ field_value_format(Key3, Key3Spec) ++ "'"
% 	end.

% make_field_values([], [], [], _IgnoredFields) -> [];
% make_field_values([FieldVal | Rest1], [FieldName | Rest2], [FieldSpec | Rest3], IgnoredFields) ->
% 	case lists:member(FieldName, IgnoredFields) of
% 		true -> 	S = "";
% 		_false -> 	S = "'" ++ field_value_format(FieldVal, FieldSpec) ++ "'"
% 	end,
% 	make_field_values(Rest1, S, Rest2, Rest3, IgnoredFields).

% make_field_values([], S, _FieldList, _FieldSpecs, _IgnoredFields) -> S;
% make_field_values([F | Rest1], S, [FieldName | Rest2], [FieldSpec | Rest3], IgnoredFields) ->
% 	case lists:member(FieldName, IgnoredFields) of
% 		true -> 	S1 = S;
% 		_false ->	S1 = S ++ ", '" ++ field_value_format(F, FieldSpec) ++ "'"
% 	end,
% 	make_field_values(Rest1, S1, Rest2, Rest3, IgnoredFields).

% make_update(Map, RecordData) ->
% 	[_, Key | FieldValues] = erlang:tuple_to_list(RecordData),
% 	[_, KeySpec | FieldSpecs] = erlang:tuple_to_list(Map#cache_config.fields_spec),
% 	case Map#cache_config.key_classic of
% 		1 -> KeyValues = [Key];
% 		_ -> KeyValues = erlang:tuple_to_list(Key)
% 	end,
% 	KeySpec1 = make_key_spec(KeySpec),
% 	WhereCond = make_where(Map#cache_config.key_fields, KeyValues, KeySpec1),
% 	[_Field | RestFields] = Map#cache_config.fields,
% 	Set = make_set(RestFields, FieldValues, FieldSpecs, Map#cache_config.ignored_fields, []),
% 	lists:concat(["UPDATE ", Map#cache_config.sql_tab, " SET ", Set, " WHERE ", WhereCond]).
	
% make_set([], [], [], _IgnoredFields, S) -> S;
% make_set([Field | Rest1], [Value | Rest2], [FieldSpec | Rest3], IgnoredFields, []) -> %% 第一次
% 	case lists:member(Field, IgnoredFields) of
% 		true ->	  S = "";
% 		_false -> S = atom_to_list(Field) ++ " = '" ++ field_value_format(Value, FieldSpec) ++ "'"
% 	end,
% 	make_set(Rest1, Rest2, Rest3, IgnoredFields, S);
% make_set([Field | Rest1], [Value | Rest2], [FieldSpec | Rest3], IgnoredFields, S) ->
% 	case lists:member(Field, IgnoredFields) of
% 		true ->	  S1 = S;
% 		_false -> S1  = S ++ ", " ++ atom_to_list(Field) ++ " = '" ++ field_value_format(Value, FieldSpec) ++ "'"
% 	end,
% 	make_set(Rest1, Rest2, Rest3, IgnoredFields, S1).
	
% field_value_format(Val, FieldSpec) ->
% 	case FieldSpec of
% 		{integer} -> integer_to_list(Val);
% 		{string} -> Val;
% 		{term} -> cache_util:term_to_string(Val)
% 	end.

% make_key_spec(KeySpec) -> 
% 	KeySpec1 = erlang:tuple_to_list(KeySpec),
% 	case length(KeySpec1) == 1 of
% 		true -> [KeySpec];
% 		false -> KeySpec1
% 	end.	
% %% 
% %% field_value_format(S, FieldSpec) when is_integer(S)->
% %%     integer_to_list(S);
% %% field_value_format(S) when is_float(S)->
% %%     float_to_list(S);
% %% field_value_format(S) ->
% %%     cache_util:term_to_string(S).
