%% @author dzR <dizengrong@gmail.com>
%% @doc 地图中玩家的的字典数据
%% 用来对从gen_cache里读出的数据做进一步的缓存，并做好了数据的更新、插入的封装

-module (role_dict).
-include ("common.hrl").
-include ("gen_cache.hrl").

-export([init/1, lookup/2, update/3, add_new/3, delete/1]).

%% @doc 初始化一个玩家的字典数据
init(RoleId) ->
	%% 需要经过该模块的gen_cache数据都预先在这里进行初始化
	%% TODO:做成可配置的，有的数据再次缓存到这里，
	%% 而有的数据不会的，这时从这里获取时将会从gen_cache里取
	[begin Data = gen_cache:lookup(get_cache_name(Tab), Tab, RoleId),
		   set_dict(RoleId, Tab, Data)
	 end || Tab <- role_dict_config:all_cached_tabs()].

-spec delete(RoleId::integer()) -> any().
%% 删除玩家的所有字典数据
delete(RoleId) ->
	[erase_dict(RoleId, Tab) || Tab <- role_dict_config:all_cached_tabs()].

-spec lookup(RoleId::integer(), Tab::atom()) -> list(). 
%% @doc 查询
lookup(RoleId, Tab) -> 
	case role_dict_config:is_cached(Tab) of
		true  -> get_dict(RoleId, Tab);
		false -> gen_cache:lookup(get_cache_name(Tab), Tab, RoleId)
	end.

-spec update(RoleId::integer(), Tab::atom(), Data::tuple() | list()) -> any().
%% @doc 更新已有的数据，参数Data可以使一个record或是list of record
update(RoleId, Tab, Data) -> 
	gen_cache:update(get_cache_name(Tab), Tab, Data),
	case role_dict_config:is_cached(Tab) of
		true  -> do_update_dict(RoleId, Tab, Data);
		false -> ok
	end.

-spec add_new(RoleId::integer(), Tab::atom(), Data::tuple() | list()) -> any().
%% @doc 插入新的数据，参数Data可以使一个record或是list of record
add_new(RoleId, Tab, Data) ->
	{ok, DataInserted} = gen_cache:insert(get_cache_name(Tab), Tab, Data),
	case role_dict_config:is_cached(Tab) of
		true  ->
			OldData = lookup(RoleId, Tab),
			Data2   = ?_IF(is_list(Data), Data ++ OldData, [Data | OldData]),
			set_dict(RoleId, Tab, Data2);
		false -> ok
	end,
	{ok, DataInserted}.

get_cache_name(Tab) -> cache_util:get_cache_name(Tab).


do_update_dict(RoleId, Tab, Data) ->
	CacheConfRec = cache_config:conf(Tab),
	NewData = case cache_config:is_multi_keys(CacheConfRec) of
		false -> Data;
		true  ->
			OldData   = lookup(RoleId, Tab),
			KeyFields = CacheConfRec#cache_config.key_fields,
			case is_list(Data) of
				false ->
					util_list:keysstore(KeyFields, OldData, Data);
				true ->
					Fun = fun(D, Acc) ->
						util_list:keysstore(KeyFields, Acc, D)
					end,
					lists:foldl(Fun, OldData, Data)
			end
	end,
	set_dict(RoleId, Tab, NewData).

%% @private
set_dict(RoleId, Tab, Data) -> erlang:put({?MODULE, Tab, RoleId}, Data).
%% @private
get_dict(RoleId, Tab) -> erlang:get({?MODULE, Tab, RoleId}).
%% @private
erase_dict(RoleId, Tab) -> erlang:erase({?MODULE, Tab, RoleId}).