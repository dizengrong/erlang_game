%% @author dzR <dizengrong@gmail.com>
%% @doc 这个定义了数据库中与玩家物品相关的表
%% 我们使用amnesia([http://amnesia.sourceforge.net])来drive我们的mysql数据库

-module (db_table_item).
-include ("item.hrl").
-include_lib ("amnesia/include/amnesia_db_def.hrl").

driver_info () -> 
	db_conf:driver_info().

tables() -> [tab_item].

table(tab_item) -> %% 玩家的物品表
	[
	 % {id, integer, [auto_increment, unique, not_null]},
	 {role_id, integer, [index, not_null]},
	 {item_id, integer, [index, not_null, {default, 0}]},	%% 玩家级别的物品唯一id
	 {type_id, integer, [not_null, {default, 0}]},			%% 物品原型id
	 {location, integer, [not_null, {default, 0}]},			%% 物品位置（1-99为装备孔位，100表示为镶嵌，从101以后为在背包中）
	 {embe_items, term, [not_null, {default, []}]},			%% 该物品镶嵌的物品id的列表[item_typeid]
	 {amount, integer, [not_null, {default, 0}]},			%% 数量
	 {start_time, integer, [not_null, {default, 0}]},		%% 如有时间限制，则为物品有效的开始时间
	 {end_time, integer, [not_null, {default, 0}]},			%% 如有时间限制，则为物品失效的结束时间
	 {morale, integer, [not_null, {default, 0}]},			%% 装备士气，战斗中会减少，要重新控制
	 {used_times, integer, [not_null, {default, 0}]},		%% 有些物品可以使用一次或多次获得里面的水晶或是怒气
	 {attr, {term, 1024}, [not_null, {default, []}]},	   	%% 物品的属性[{属性类别, [{属性code, 属性值}]}]
	 {is_bind, bool, [not_null, {default, true}]},			%% 是否绑定
	 {firm_exp, integer, [not_null, {default, 0}]},			%% 当前强化经验
	 {firm_lv, integer, [not_null, {default, 0}]},			%% 当前强化等级
	 {endurance, integer, [not_null, {default, 0}]},		%% 当前耐久度
	 {colour, integer, [not_null, {default, ?ITEM_COLOUR_WHITE}]}	%% 当前颜色品质
	];

table(Tab) -> erlang:throw({?MODULE, error_confg, 'table/1', Tab}).