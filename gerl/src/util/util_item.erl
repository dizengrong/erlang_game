%% @author dzR <dizengrong@gmail.com>
%% @doc 与物品相关的一些util方法

-module (util_item).
-include ("item.hrl").

-export ([can_overlap/1, get_item_name/1]).
-export ([get_item_conf_colour/1, get_item_conf_bind/1, get_item_conf_lifetime/1]).
-export ([get_item_conf_firm_lv/1, get_item_conf_attr/1]).
-export ([is_in_bag/1, is_equiped/1, is_embeded/1, location_type/1]).

-spec can_overlap(TypeId::non_neg_integer()) -> boolean().
%% @doc 返回物品是否可以堆叠
can_overlap(TypeId) ->
	CItemRec = cfg_item:get(TypeId),
	CItemRec#c_item.is_overlap.

-spec get_item_name(TypeId::non_neg_integer()) -> string().
%% @doc 获取物品的名称
get_item_name(TypeId) ->
	CItemRec = cfg_item:get(TypeId),
	CItemRec#c_item.name.

%% ================ get_item_conf_xxx/1 获取物品配置 ================
%% @doc 获取物品配置的颜色，颜色值定义 参见头文件:item.hrl
get_item_conf_colour(TypeId) ->
	CItemRec = cfg_item:get(TypeId),
	CItemRec#c_item.colour.

get_item_conf_bind(TypeId) ->
	CItemRec = cfg_item:get(TypeId),
	CItemRec#c_item.is_bind.

get_item_conf_firm_lv(TypeId) ->
	CItemRec = cfg_item:get(TypeId),
	CItemRec#c_item.firm_lv.

get_item_conf_attr(TypeId) ->
	CItemRec = cfg_item:get(TypeId),
	CItemRec#c_item.attr.

get_item_conf_lifetime(TypeId) ->
	CItemRec = cfg_item:get(TypeId),
	CItemRec#c_item.lifetime.
%% ================ get_item_conf_xxx/1 获取物品配置 ================


%% @doc 根据物品的location判定是否是在背包中
is_in_bag(Location) -> (Location > ?ITEM_LOCATION_EMBE).
%% @doc 根据物品的location判定是否是在装备在身上
is_equiped(Location) -> (Location < ?ITEM_LOCATION_EMBE).
%% @doc 根据物品的location判定是否是嵌入在其他物品上的
is_embeded(Location) -> (Location == ?ITEM_LOCATION_EMBE).

%% @doc 根据物品的location获取其所在位置的类型
%% in_bag:在背包中
%% equiped:装备在装备槽位上
%% embeded:镶嵌在其他物品上
location_type(Location) -> 
	if
		Location > ?ITEM_LOCATION_EMBE -> in_bag;
		Location < ?ITEM_LOCATION_EMBE -> equiped;
		true -> embeded
	end.


