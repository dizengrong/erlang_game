%% @author dzR <dizengrong@gmail.com>
%% @doc 玩家背包字典数据模块

-module (role_bag_dict).
-include ("db_table_item.hrl").
-include ("item.hrl").
-include ("common.hrl").
-include ("spec_type.hrl").

-export ([init/1, delete/1, add_new/2]).
-export ([get_item/2, set_item/1, delete_item/2, get_eqiuped_equip/2]).
-export ([set_used_capacity/2, get_used_capacity/1]).
-export ([filter_by_type_id/2]).
-export ([set_max_item_id/2, increase_max_item_id/1]).
-export ([use_a_free_bag_location/1]).
-export ([get_lifetime_index/1]).

-define(TAB_ITEM, 	tab_item).
-define(ITEM_CACHE_SERVER,	cache_util:get_cache_name(?TAB_ITEM)).

init(RoleId) ->
	set_equiped_indexs(RoleId, []),
	set_all_item_index(RoleId, []),
	Datas = gen_cache:lookup(?ITEM_CACHE_SERVER, ?TAB_ITEM, RoleId),
	NowSeconds = util_time:now_seconds(),
	Fun = fun(Rec, {MaxId, Locations}) ->
		RItemRec = init_item(Rec),
		NewMaxId = max(MaxId, RItemRec#r_item.item_id),
		init_lifetime(RoleId, RItemRec, NowSeconds),
		NewLocations = case util_item:is_in_bag(RItemRec#r_item.location) of
			true -> [RItemRec#r_item.location | Locations];
			false -> Locations
		end,
		{NewMaxId, NewLocations}
	end,
	{MaxItemId, UsedLocations} = lists:foldl(Fun, {1, []}, Datas),
	set_max_item_id(RoleId, MaxItemId),
	set_used_capacity(RoleId, length(Datas) - length(get_equiped_indexs(RoleId))),
	init_free_bag_locations(RoleId, UsedLocations),
	ok.

init_lifetime(RoleId, RItemRec, NowSeconds) ->
	case RItemRec#r_item.start_time > 0 of
		true -> %% 有使用时间的
			case RItemRec#r_item.end_time < NowSeconds of
				true  -> add_to_lifetime_index(RoleId, RItemRec#r_item.item_id);
				false -> ok
			end;
		false -> ok
	end.

%% @doc 初始化背包空闲的位置队列
init_free_bag_locations(RoleId, UsedLocations) ->
	TotalCapactiy = role_bag:get_bag_capacity(RoleId),
	FreeLocations = lists:seq(1, TotalCapactiy) -- UsedLocations,
	Q             = queue:from_list(FreeLocations),
	erlang:put({?MODULE, free_bag_locations, RoleId}, Q).

%% @doc 添加一个可用的背包位置到空闲位置队列
add_free_bag_location(RoleId, Location, InsertAtFront) ->
	set_used_capacity(RoleId, get_used_capacity(RoleId) + 1),
	Q1 = erlang:get({?MODULE, free_bag_locations, RoleId}),
	Q2 = case InsertAtFront of
		true  -> queue:in_r(Location, Q1);
		false -> queue:in(Location, Q1)
	end,
	erlang:put({?MODULE, free_bag_locations, RoleId}, Q2).



%% @doc 删除玩家背包的所有字典数据
delete(RoleId) ->
	_ = [erase_item(RoleId, ItemId) || ItemId <- get_all_item_index(RoleId)],
	erlang:erase({?MODULE, all_item_index, RoleId}),
	erlang:erase({?MODULE, equiped_index, RoleId}),
	erlang:erase({?MODULE, use_capacity, RoleId}),
	erlang:erase({?MODULE, max_item_id, RoleId}),
	erlang:erase({?MODULE, free_bag_locations, RoleId}),
	erlang:erase({?MODULE, lifetime_index, RoleId}),
	ok.

-spec delete_item(RoleId::role_id(), ItemId::integer()) -> ok.
%% @doc 根据物品id删除该物品
%% 注意：若该物品是镶嵌在其他物品上的，这里是不会删除它在宿主物品镶嵌列表中的引用的
delete_item(RoleId, ItemId) ->
	RItemRec = erase_item(RoleId, ItemId),
	delete_from_all_item_index(RoleId, ItemId),
	delete_from_lifetime_index(RoleId, ItemId),
	%% 注意物品tab_item表在gen_cache中的关键字为RoleId和ItemId，
	%% 具体可以参加cache_config:conf/1中有关该表的配置
	gen_cache:delete(?ITEM_CACHE_SERVER, ?TAB_ITEM, [RoleId, ItemId]),
	Location = RItemRec#r_item.location,
	case util_item:location_type(Location) of
		equiped -> delete_from_equiped_index(RoleId, ItemId);
		in_bag  -> add_free_bag_location(RoleId, Location, true);
		embeded -> ok
	end,
	ok.

-spec add_new(RoleId::role_id(), TabItemRecs::[#tab_item{}]) -> {ok, [#r_item{}]}.
%% @doc 增加的新物品，会直接插入到数据库的
add_new(_RoleId, TabItemRecs) ->
	{ok, DataAdded} = gen_cache:insert(?ITEM_CACHE_SERVER, ?TAB_ITEM, TabItemRecs),
	RItemRecs = [init_item(Rec) || Rec <- DataAdded],
	{ok, RItemRecs}.

%% @doc 获取并使用一个空闲的背包位置，调用者要保证有空闲的位置
use_a_free_bag_location(RoleId) ->
	Q = erlang:get({?MODULE, free_bag_locations, RoleId}),
	{{value, Location}, Q2} = queue:out(Q),
	erlang:put({?MODULE, free_bag_locations, RoleId}, Q2),
	set_used_capacity(RoleId, get_used_capacity(RoleId) + 1),
	Location.

%% @doc 操作已使用的背包容量的方法
set_used_capacity(RoleId, UsedCapacity) ->
	erlang:put({?MODULE, use_capacity, RoleId}, UsedCapacity).
get_used_capacity(RoleId) ->
	erlang:get({?MODULE, use_capacity, RoleId}).

-spec filter_by_type_id(RoleId::role_id(), TypeId::integer()) -> [#r_item{}].
%% @doc 过滤出指定物品类型id的所有物品
filter_by_type_id(RoleId, TypeId) ->
	AllItemId = get_all_item_index(RoleId),
	filter_by_type_id(RoleId, AllItemId, TypeId, []).

filter_by_type_id(_RoleId, [], _TypeId, Acc) -> Acc;
filter_by_type_id(RoleId, [ItemId | Rest], TypeId, Acc) ->
	{ok, RItemRec} = get_item(RoleId, ItemId),
	Acc2 = ?_IF(RItemRec#r_item.type_id == TypeId, [RItemRec | Acc], Acc),
	filter_by_type_id(RoleId, Rest, TypeId, Acc2).

init_item(TabItemRec) ->
	RItemRec = #r_item{
		id         = TabItemRec#tab_item.id,
		role_id    = TabItemRec#tab_item.role_id,
		item_id    = TabItemRec#tab_item.item_id,
		type_id    = TabItemRec#tab_item.type_id,
		location   = TabItemRec#tab_item.location,
		embe_items = TabItemRec#tab_item.embe_items,
		amount     = TabItemRec#tab_item.amount,
		start_time = TabItemRec#tab_item.start_time,
		end_time   = TabItemRec#tab_item.end_time,
		morale     = TabItemRec#tab_item.morale,
		used_times = TabItemRec#tab_item.used_times,
		attr       = TabItemRec#tab_item.attr,
		is_bind    = TabItemRec#tab_item.is_bind,
		firm_exp   = TabItemRec#tab_item.firm_exp,
		firm_lv    = TabItemRec#tab_item.firm_lv,
		endurance  = TabItemRec#tab_item.endurance,
		colour     = TabItemRec#tab_item.colour
	},
	RoleId = TabItemRec#tab_item.role_id,
	ItemId = TabItemRec#tab_item.item_id,
	erlang:put({?MODULE, item, RoleId, ItemId}, RItemRec),
	add_to_all_item_index(RoleId, ItemId),
	case util_item:is_equiped(RItemRec#r_item.location) of
		true  -> add_to_equiped_index(RItemRec);
		false -> ok
	end,
	RItemRec.

%% @doc 操作已装备的装备列表
%% EquipedIndexs: [{item_id, location}]
set_equiped_indexs(RoleId, EquipedIndexs) -> 
	erlang:put({?MODULE, equiped_index, RoleId}, EquipedIndexs).
get_equiped_indexs(RoleId) -> 
	erlang:get({?MODULE, equiped_index, RoleId}).
add_to_equiped_index(RItemRec) ->
	Indexs  = get_equiped_indexs(RItemRec#r_item.role_id),
	Indexs2 = [{RItemRec#r_item.item_id, RItemRec#r_item.location} | Indexs],
	set_equiped_indexs(RItemRec#r_item.role_id, Indexs2).
delete_from_equiped_index(RoleId, ItemId) ->
	Indexs  = get_equiped_indexs(ItemId),
	Indexs2 = lists:keydelete(ItemId, 1, Indexs),
	set_equiped_indexs(RoleId, Indexs2).

-spec get_item(RoleId::role_id(), ItemId::integer()) 
	-> {error, no_this_item, ItemId::integer()} | {ok, #r_item{}}.
%% @doc 获取背包中的物品
get_item(RoleId, ItemId) ->
	case erlang:get({?MODULE, item, RoleId, ItemId}) of
		Rec when is_record(Rec, r_item) -> {ok, Rec};
		_ -> {error, no_this_item, ItemId}
	end.
%% @doc 设置缓存中的物品数据，调用者必须保证物品已在缓存中了
set_item(RItemRec) ->
	RoleId  = RItemRec#r_item.role_id,
	ItemId  = RItemRec#r_item.item_id,
	{ok, _} =  get_item(RoleId, ItemId),
	erlang:put({?MODULE, item, RoleId, ItemId}, RItemRec).
erase_item(RoleId, ItemId) ->
	RItemRec = erlang:erase({?MODULE, item, RoleId, ItemId}),
	RItemRec.

%% @doc 操作所有物品id的索引, Indexs:[item_id]
set_all_item_index(RoleId, Indexs) ->
	erlang:put({?MODULE, all_item_index, RoleId}, Indexs).
get_all_item_index(RoleId) ->
	erlang:get({?MODULE, all_item_index, RoleId}).
add_to_all_item_index(RoleId, ItemId) ->
	Indexs = get_all_item_index(RoleId),
	set_all_item_index(RoleId, [ItemId | Indexs]).
delete_from_all_item_index(RoleId, ItemId) ->
	Indexs = get_all_item_index(RoleId),
	set_all_item_index(RoleId, lists:delete(ItemId, Indexs)).


-spec get_eqiuped_equip(RoleId::role_id(), Location::integer()) 
	-> {error, no_equip_at_this_location, Location::integer()} | 
	   {error, no_this_item, ItemId::integer()} | 
	   {ok, #r_item{}}.
%% @doc 通过装备位置获取装备上的装备
get_eqiuped_equip(RoleId, Location) ->
	Indexs  = get_equiped_indexs(RoleId),
	case lists:keyfind(Location, 2, Indexs) of
		false -> {error, no_equip_at_this_location, Location};
		{ItemId, _} ->
			get_item(RoleId, ItemId)
	end.

%% @doc 操作玩家当前最大的物品id的方法
set_max_item_id(RoleId, MaxItemId) ->
	erlang:put({?MODULE, max_item_id, RoleId}, MaxItemId).
increase_max_item_id(RoleId) ->
	MaxItemId = erlang:get({?MODULE, max_item_id, RoleId}) + 1,
	set_max_item_id(RoleId, MaxItemId),
	MaxItemId.

%% @doc 操作有使用时间限制的物品的索引
add_to_lifetime_index(RoleId, ItemId) ->
	set_lifetime_index(RoleId, [ItemId | get_lifetime_index(RoleId)]).
delete_from_lifetime_index(RoleId, ItemId) ->
	set_lifetime_index(RoleId, lists:delete(ItemId, get_lifetime_index(RoleId))).

get_lifetime_index(RoleId) ->
	erlang:get({?MODULE, lifetime_index, RoleId}).
set_lifetime_index(RoleId, Indexs) ->
	erlang:put({?MODULE, lifetime_index, RoleId}, Indexs).


