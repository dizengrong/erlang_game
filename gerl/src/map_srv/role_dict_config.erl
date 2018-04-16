%% @author dzR <dizengrong@gmail.com>
%% @doc 配置地图中玩家的的字典数据的缓存，决定哪些表需要在role_dict中进一步缓存

-module (role_dict_config).
-compile([export_all]).

%% @doc 所有的需要缓存的表
all_cached_tabs() ->
	[Tab || Tab <- db_module:tables(), is_cached(Tab)].

%% @doc 表TableName是否被缓存了
is_cached(tab_item) -> false; %% 物品表将由物品模块的role_bag_dict.erl管理
is_cached(_TableName) -> true.

