%% @doc 生成record的字段索引到字段名称的映射方法
-module (gen_record_fields).
-compile([export_all]).

-define(HRL_FILES, ["include/db_tables.hrl"]).
-define(MODULE_NAME, "record_fields").

start() ->
	Fun = fun(HrlFile, Acc) -> 
		Src = make(HrlFile),
		Src ++ Acc
	end,
	Str = lists:foldl(Fun, "", ?HRL_FILES),
	DestFile = "src/" ++ ?MODULE_NAME ++ ".erl",
	ok  = file:write_file(DestFile, list_to_binary(Str)),
	io:format("Create file ~s successfully!\n", [DestFile]).

make(HrlFile) ->
	{ok, Tree} = epp:parse_file(HrlFile, ["./"], []),
	Src = make_src(Tree),
	Src.

make_src(Tree) -> make_src(Tree,[]).    

make_src([],Acc)                              -> make_src2(Acc,[],[]);
make_src([{attribute,_,record,Record}|T],Acc) -> make_src(T,[Record|Acc]);
make_src([_H|T],Acc)                          -> make_src(T,Acc).
 
make_src2([],Acc1,Acc2)    -> top_and_tail(Acc1,Acc2);
make_src2([H|T],Acc1,Acc2) -> 
	{NewAcc1,NewAcc2}=expand_rec(H),
	make_src2(T,[NewAcc1|Acc1],[NewAcc2|Acc2]).
 
expand_rec({Name,Def}) -> expand_fields(Name,Def,1,[]).
 
expand_fields(Name,[],N,Acc) -> {mk2(Name,N-1),lists:reverse("\n" ++ Acc)};
expand_fields(Name, [{record_field,_,{atom,_,F},_}|T], N, Acc) -> 
	expand_fields(Name,T,N+1,[mk(Name,F,N)|Acc]);
expand_fields(Name,[{record_field,_,{atom,_,F}}|T],N,Acc) -> 
	expand_fields(Name,T,N+1,[mk(Name,F,N)|Acc]);
expand_fields(Name,[_H|T],N,Acc) -> expand_fields(Name,T,N+1,Acc).
 
%% mk2/1 builds the no of fields fns
mk2(Name,N) -> "no_of_fields("++atom_to_list(Name)++") -> "++
		   integer_to_list(N)++";\n".
 
mk(Name,Field,N) -> 
	"get_field_name("++atom_to_list(Name)++", "++
	integer_to_list(N + 1)++")-> "++atom_to_list(Field)++";\n".
 
top_and_tail(Acc1,Acc2)->
	Top="%% This module automatically generated - do not edit\n"++
	"%% This module provides utilities for mapping field index to field name\n"++
	"%% get_field_name/2 get the field atom name by record and it's field index number\n" ++
	"%% no_of_fields/1 return how many fields in the record\n" ++
	"\n"++
	"-module("++?MODULE_NAME++").\n"++
	"\n"++
	"-export([get_field_name/2,no_of_fields/1]).\n"++
	"\n",
	Tail1="no_of_fields(Other) -> exit({error,\"Invalid Record Name: \""++
	"++Other}).\n\n\n",
	Tail2="get_field_name(Record,Field) -> exit({error,\""++
	"Invalid Record Name and Field: \" ++ {Record, Field}}).\n",
	Top++lists:flatten(Acc1)++Tail1++lists:flatten(Acc2)++Tail2.