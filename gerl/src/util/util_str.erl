%% @author dzR <dizengrong@gmail.com>
%% @doc 与字符串相关的一些方法

-module (util_str).
-export([term_to_str/1, str_to_term/1]).


-spec term_to_str(tuple()) -> string().
%% @doc term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_str(Term) ->
	lists:flatten(io_lib:format("~w", [Term])).

-spec str_to_term(string()) -> tuple().
%% @doc term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
%% 由调用者处理错误
str_to_term(String) ->
    {ok, Tokens, _} = erl_scan:string(String ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
	Term.
