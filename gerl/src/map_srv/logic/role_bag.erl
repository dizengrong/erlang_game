%% @author dzR <dizengrong@gmail.com>
%% @doc 玩家背包模块

-module (role_bag).
-include ("item.hrl").
-include ("bag.hrl").
-include ("common.hrl").
-include ("error_code.hrl").
-include ("spec_type.hrl").
-include ("db_table_item.hrl").
-include ("db_table_role.hrl").

-export ([create_item/2, get_bag_capacity/1]).
-export ([swap_item/3]).


-spec create_item(RoleId::role_id(), CreateParams::#r_create_item{}) ->
	{error, error_code()} | {ok, [#r_item{}]}.
%% @doc 创建物品，失败返回:{error, ErrCode}，成功返回:{ok, 更新的物品列表}
create_item(RoleId, CreateParams) ->
	case check_create(RoleId, CreateParams) of
		true ->
			TypeId = CreateParams#r_create_item.type_id,
			{ok, UpdatedList} = case util_item:can_overlap(TypeId) of
				true -> 
					create_overlap_item(RoleId, CreateParams);
				false ->
					create_no_overlap_item(RoleId, CreateParams)
			end,
			%% todo:通知客户端物品的变动
			%% 记录道具日志
			util_user_log:log_item(RoleId, CreateParams),
			{ok, UpdatedList};
		{error, ErrCode} -> {error, ErrCode}
	end.

-spec swap_item(RoleId::role_id(), ItemId::integer(), DestLoc::integer()) ->
	{error, error_code()} | {ok, undefined|#r_item{}, undefined|#r_item{}}.
%% @doc 交换物品，将物品ItemId移动到目的位置DestLoc，有如下3中情况
%% 1）目的位置上无物品，只是将物品1移动到目的位置了
%% 2）对调，物品1被移动目的位置，而目的位置上的物品被移动到物品1原来的位置上
%% 3）合并，目的位置上的物品可以和物品1合并，根据最大堆叠数可能只合并一部分
swap_item(RoleId, ItemId, DestLoc) ->
	case check_swap(RoleId, ItemId, DestLoc) of
		{error, ErrCode} -> {error, ErrCode};
		{true, RItemRec1} -> 
			case role_bag_dict:get_item_by_location(RoleId, DestLoc) of
				{ok, RItemRec2} ->
					case util_item:can_overlap(RItemRec2#r_item.type_id) of
						false -> swap_item_without_overlap(RItemRec1, RItemRec2);
						true  -> swap_item_with_overlap(RoleId, RItemRec1, RItemRec2)
					end;
				_ ->
					RItemRec11 = RItemRec1#r_item{location = DestLoc},
					role_bag_dict:set_item(RItemRec11),
					{ok, undefined, RItemRec11}
			end
	end.

%% @doc 仅仅对调物品
swap_item_without_overlap(RItemRec1, RItemRec2) ->
	SrcLoc     = RItemRec1#r_item.location,
	DestLoc    = RItemRec2#r_item.location,
	RItemRec11 = RItemRec1#r_item{location = DestLoc},
	RItemRec22 = RItemRec2#r_item{location = SrcLoc},
	role_bag_dict:set_item(RItemRec11),
	role_bag_dict:set_item(RItemRec22),
	{ok, RItemRec22, RItemRec11}.

%% @doc RItemRec1将RItemRec1合并到RItemRec2
swap_item_with_overlap(RoleId, RItemRec1, RItemRec2) ->
	Total = RItemRec1#r_item.amount + RItemRec2#r_item.amount,
	case Total > ?MAX_OVERLAP_AMOUNT of
		true ->
			RItemRec11 = RItemRec1#r_item{amount = Total - ?MAX_OVERLAP_AMOUNT},
			RItemRec22 = RItemRec2#r_item{amount = ?MAX_OVERLAP_AMOUNT},
			role_bag_dict:set_item(RItemRec11),
			role_bag_dict:set_item(RItemRec22);
		false ->
			RItemRec11 = undefined,
			RItemRec22 = RItemRec2#r_item{amount = Total},
			role_bag_dict:set_item(RItemRec22),
			role_bag_dict:delete_item(RoleId, RItemRec1#r_item.item_id)
	end,
	{ok, RItemRec11, RItemRec22}.

create_no_overlap_item(RoleId, CreateParams) ->
	CreateAmount = CreateParams#r_create_item.amount,
	TabItemRecs  = [make_new_item_rec(RoleId, CreateParams, 1) 
					|| _ <- lists:seq(1, CreateAmount)],
	{ok, RItemRecs} = role_bag_dict:add_new(RoleId, TabItemRecs),
	{ok, RItemRecs}.

%% @doc 创建可以堆叠的物品，
%% 注意：可堆叠的物品一定都是无其他差别的
%% 所以创建时才可以简单的增加已存在的物品的数量的
create_overlap_item(RoleId, CreateParams) ->
	TypeId       = CreateParams#r_create_item.type_id,
	CreateAmount = CreateParams#r_create_item.amount,
	ExistItems   = role_bag_dict:filter_by_type_id(RoleId, TypeId),
	{LeftAmount, UpdatedList} = add_to_exist_item(ExistItems, CreateAmount, []),
	case LeftAmount =< 0 of
		true ->
			{ok, UpdatedList};
		false ->
			TabItemRecs = create_overlap_item_help(RoleId, CreateParams, LeftAmount, []),
			{ok, RItemRecs} = role_bag_dict:add_new(RoleId, TabItemRecs),
			{ok, RItemRecs ++ UpdatedList}
	end.			

create_overlap_item_help(_RoleId, _CreateParams, 0, TabItemRecs) -> 
	TabItemRecs;
create_overlap_item_help(RoleId, CreateParams, LeftAmount, TabItemRecs) ->
	Amount = min(?MAX_OVERLAP_AMOUNT, LeftAmount),
	Rec = make_new_item_rec(RoleId, CreateParams, Amount),
	LeftAmount2 = LeftAmount - Amount,
	TabItemRecs2 = [Rec | TabItemRecs],
	create_overlap_item_help(RoleId, CreateParams, LeftAmount2, TabItemRecs2).

add_to_exist_item(_, 0, UpdatedRItemRecs) -> 
	{0, UpdatedRItemRecs};
add_to_exist_item([], LeftAmount, UpdatedRItemRecs) -> 
	{LeftAmount, UpdatedRItemRecs};
add_to_exist_item([RItemRec | Rest], LeftAmount, UpdatedRItemRecs) ->
	ExistAmount = RItemRec#r_item.amount,
	Add = ?MAX_OVERLAP_AMOUNT - ExistAmount,
	case Add =< 0 of
		true  -> add_to_exist_item(Rest, LeftAmount, UpdatedRItemRecs);
		false ->
			Add2 = min(Add, LeftAmount),
			RItemRec2 = RItemRec#r_item{amount = ExistAmount + Add2},
			role_bag_dict:set_item(RItemRec2),
			UpdatedRItemRecs2 = [RItemRec2 | UpdatedRItemRecs],
			add_to_exist_item(Rest, LeftAmount - Add2, UpdatedRItemRecs2)
	end.

%% @doc 创建物品时构造一个#tab_item{}
make_new_item_rec(RoleId, CreateParams, Amount) ->
	TypeId       = CreateParams#r_create_item.type_id,
	{Start, End} = get_create_param(TypeId, lifetime, CreateParams#r_create_item.lifetime),
	#tab_item{
		role_id    = RoleId,
		item_id    = role_bag_dict:increase_max_item_id(RoleId),
		type_id    = TypeId,
		location   = role_bag_dict:use_a_free_bag_location(RoleId),
		embe_items = [],
		amount     = Amount,
		start_time = Start,
		end_time   = End,
		morale     = ?DEFAULT_MORALE,
		used_times = 0,	%% todo:这个字段暂时处理，以后考虑从配置爱中读取
		attr       = get_create_param(TypeId, attr, CreateParams#r_create_item.attr),
		is_bind    = get_create_param(TypeId, is_bind, CreateParams#r_create_item.is_bind),
		firm_exp   = 0,
		firm_lv    = get_create_param(TypeId, firm_lv, CreateParams#r_create_item.firm_lv),
		endurance  = ?ENDURANCE_NEVER, %% todo:暂时不处理耐久度
		colour     = get_create_param(TypeId, colour, CreateParams#r_create_item.colour)
	}.

%% @doc 创建道具的各个字段参数的处理
get_create_param(TypeId, is_bind, default) -> 
	util_item:get_item_conf_bind(TypeId);
get_create_param(_TypeId, is_bind, IsBind) -> IsBind;

get_create_param(TypeId, firm_lv, default) -> 
	util_item:get_item_conf_firm_lv(TypeId);
get_create_param(_TypeId, firm_lv, FirmLv) -> FirmLv;

get_create_param(TypeId, colour, default) -> 
	util_item:get_item_conf_colour(TypeId);
get_create_param(_TypeId, colour, FirmLv) -> FirmLv;

get_create_param(TypeId, attr, default) -> 
	util_item:get_item_conf_attr(TypeId);
get_create_param(_TypeId, attr, Attr) -> Attr;

get_create_param(TypeId, lifetime, default) -> 
	case util_item:get_item_conf_lifetime(TypeId) of
		?LIFETIME_ENDLESS -> {0, 0};
		Lifetime -> 
			NowSecs  = util_time:now_seconds(),
			{NowSecs, NowSecs + Lifetime}
	end;
get_create_param(_TypeId, lifetime, Lifetime) ->
	NowSecs = util_time:now_seconds(),
	{NowSecs, NowSecs + Lifetime}.	


check_create(RoleId, CreateParams) ->
	NeedFreeBag  = calc_need_free_bag(RoleId, CreateParams),
	LeftCapacity = get_bag_capacity(RoleId) - role_bag_dict:get_used_capacity(RoleId),
	?_IF(LeftCapacity >= NeedFreeBag, true, {error, ?ERR_BAG_INSUFFICIENT}).

check_swap(RoleId, ItemId, DestLoc) ->
	BagCapacity = role_bag_dict:get_bag_capacity(RoleId),
	case DestLoc > ?ITEM_LOCATION_EMBE andalso 
		 DestLoc =< BagCapacity + (?ITEM_LOCATION_EMBE + 1) of
		true -> 
			case srole_bag_dict:get_item(RoleId, ItemId) of
				{ok, RItemRec} -> {true, RItemRec};
				{error, no_this_item, _} -> {error, ?ERR_BAG_NO_THIS_ITEM}
			end;
		false ->
			{error, ?ERR_BAG_SWAP_BAD_PARAM}
	end.

%% @doc 计算需要的空闲空间
calc_need_free_bag(RoleId, CreateParams) ->
	TypeId       = CreateParams#r_create_item.type_id,
	CreateAmount = CreateParams#r_create_item.amount,
	NeedFreeBag = case util_item:can_overlap(TypeId) of
		false -> CreateAmount;
		true  ->
			ExistItems = role_bag_dict:filter_by_type_id(RoleId, TypeId),
			FreeOverlap = get_free_overlap_num(ExistItems),
			?_IF(CreateAmount =< FreeOverlap, 0, 
				 util_math:ceil_div(CreateAmount - FreeOverlap, ?MAX_OVERLAP_AMOUNT))
	end,
	NeedFreeBag.

-spec get_free_overlap_num(ExistItems::[#r_item{}]) -> non_neg_integer().
%% @doc 获取可堆叠物品的剩余可堆叠数
get_free_overlap_num(ExistItems) ->
	get_free_overlap_num(ExistItems, 0).
get_free_overlap_num([], Acc) -> Acc;
get_free_overlap_num([RItemRec | Rest], Acc) ->
	get_free_overlap_num(Rest, Acc + ?MAX_OVERLAP_AMOUNT - RItemRec#r_item.amount).

get_bag_capacity(RoleId) ->
	[RoleDataRec | _] = role_dict:lookup(RoleId, tab_role_data),
	RoleDataRec#tab_role_data.bag_capacity.