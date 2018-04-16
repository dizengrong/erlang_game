%% @author dzR <dizengrong@gmail.com>
%% @doc 有生命期限的物品的生命期处理

-module (item_life).
-include ("item.hrl").
-include ("spec_type.hrl").

-export ([check_lifetime/2]).

%% @doc 对玩家有使用期限的物品进行检测
check_lifetime(RoleId, NowSeconds) ->
	_ = [do_check(RoleId, ItemId, NowSeconds) 
	 	 || ItemId <- role_bag_dict:get_lifetime_index(RoleId)],
	 ok.

do_check(RoleId, ItemId, NowSeconds) ->
	RItemRec = role_bag_dict:get_item(RoleId, ItemId),
	case RItemRec#r_item.end_time > NowSeconds of
		true -> 
			%% todo:过期了，要做过期的处理
			ok;
		false -> ok
	end.

