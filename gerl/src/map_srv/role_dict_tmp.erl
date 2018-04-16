%% @author dzR <dizengrong@gmail.com>
%% @doc 地图中玩家的的临时字典数据
%% 本模块的主要作用是保存一些临时的字典数据
%% 与role_dict模块不同的是，role_dict_tmp不会和gen_cache挂钩
%% 对数据表的数据操作都应该通过role_dict来与gen_cache打交道
%% 但是对于一些需要频繁操作的数据可以在临时性的放到这里，
%% 然后在一定的时候通过role_dict写入gen_cache
%% 重要：不要将undefined做为字典数据key所对应的value值来设置
%% 因为本模块依据undefined做了一些实现细节上的处理

-module (role_dict_tmp).

-export([init/1, get/2, set/3, delete/1]).

init(RoleId) ->
	erlang:put({tmp_dict_all_keys, RoleId}, []),
	ok.

-spec get(RoleId::integer(), Key::term()) -> term(). 
%% @doc 查询
get(RoleId, Key) -> 
	erlang:get({tmp_dict, RoleId, Key}).

-spec delete(RoleId::integer()) -> any().
%% 删除玩家的所有临时字典数据
delete(RoleId) ->
	[erlang:erase({tmp_dict, RoleId, Key}) || Key <- erlang:get({tmp_dict_all_keys, RoleId})].

-spec set(RoleId::integer(), Key::term(), Data::term()) -> any().
%% @doc 设置字典数据
set(RoleId, Key, Data) -> 
	case get(RoleId, Key) of
		undefined -> add_to_dict_key(RoleId, Key);
		_ -> ok
	end,
	erlang:put({tmp_dict, RoleId, Key}, Data).

add_to_dict_key(RoleId, Key) ->
	DictKeys = erlang:get({tmp_dict_all_keys, RoleId}),
	erlang:put({tmp_dict_all_keys, RoleId}, [Key | DictKeys]).

