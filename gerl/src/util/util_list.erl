%% @author dzR <dizengrong@gmail.com>
%% @doc 与list相关的一些方法

-module (util_list).
-export([keysstore/3]).


-spec keysstore(KeyFields::[integer()],
				TupleLists::[tuple()], 
				NewTuple::tuple()) -> [tuple()].
%% @doc 将NewTuple存储到TupleLists中
%% 如果NewTuple与TupleLists中某一个元素的所有KeyFields相匹配
%% 没有的话，则NewTuple被添加到TupleLists的末尾
%% 与lists:keystore的不同在于这里是对多个key域进行对比
%% 如: util_list:keysstore([1,2], [{11,22,aaa},{10,20,bbb}], {10,20,ccc})
%% 则结果为: [{11,22,aaa},{10,20,ccc}]
keysstore(KeyFields, TupleLists, NewTuple) ->
	keysstore_help(KeyFields, NewTuple, TupleLists, []).

keysstore_help(_KeyFields, NewTuple, [], Acc) -> lists:append(Acc, [NewTuple]);
keysstore_help(KeyFields, NewTuple, [Tuple | Rest], Acc) ->
	case is_keys_matched(KeyFields, NewTuple, Tuple) of
		true  -> Acc ++ [NewTuple | Rest];
		false -> keysstore_help(KeyFields, NewTuple, Rest, [Tuple | Acc])
	end.

is_keys_matched([], _Tuple1, _Tuple2) -> true;
is_keys_matched([KeyField | Rest], Tuple1, Tuple2) ->
	case element(KeyField, Tuple1) == element(KeyField, Tuple2) of
		true  -> is_keys_matched(Rest, Tuple1, Tuple2);
		false -> false
	end.
