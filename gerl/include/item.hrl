%% @doc 物品定义相关的头文件

%% 物品颜色定义
-define(ITEM_COLOUR_WHITE,	1).		%% 白色
-define(ITEM_COLOUR_GREEN,	2).		%% 绿色
-define(ITEM_COLOUR_BLUE,	3).		%% 蓝色
-define(ITEM_COLOUR_PURPLE,	4).		%% 紫色
-define(ITEM_COLOUR_ORANGE,	5).		%% 橙色
-define(ITEM_COLOUR_GOLDEN,	6).		%% 金色

%% 所有的物品类型
-define(ALL_ITEM_KIND,		[1,2,3]).
%% 物品类型定义
-define(ITEM_KIND_MEDICINE,	1).		%% 药水
-define(ITEM_KIND_MATERIAL,	2).		%% 材料
-define(ITEM_KIND_EQUIP,	3).		%% 装备

%% 物品位置定义
% 武器位置
-define(ITEM_LOCATION_HELMET,		1).		%% 头盔
-define(ITEM_LOCATION_ARMOR,		2).		%% 护甲
-define(ITEM_LOCATION_BELT,			3).		%% 腰带
-define(ITEM_LOCATION_SHOES,		4).		%% 鞋子
-define(ITEM_LOCATION_WEAPON,		5).		%% 武器
-define(ITEM_LOCATION_SHIELD,		6).		%% 护盾
-define(ITEM_LOCATION_Badge,		7).		%% 徽章
% 指明该物品被镶嵌了，从101开始表示为在背包中的位置
-define(ITEM_LOCATION_EMBE,			100).

%% 物品中的装备类的耐久度
-define(ENDURANCE_NEVER, 	-1).	%% 表示物品没有耐久度，不会损坏
-define(DEFAULT_ENDURANCE,	100).	%% 默认的物品耐久度

%% 物品的寿命
-define(LIFETIME_ENDLESS,	-1).	%% 表示物品没有使用期限或无限期使用

%% 默认的物品士气
-define(DEFAULT_MORALE,		100).

%% 物品数据record，具体字段的意思参见tab_item record
-record (r_item, {
	id         = 0,
	role_id    = 0,
	item_id    = 0,
	type_id    = 0,
	location   = 0,
	embe_items = 0,
	amount     = 0,
	start_time = 0,
	end_time   = 0,
	morale     = 0,
	used_times = 0,
	attr       = [], 
	is_bind    = true,
	firm_exp   = 0,
	firm_lv    = 0,
	endurance  = 0,
	colour     = 0
}).

%% 创建物品的参数record
-record (r_create_item, {
	type_id  = 0,			%% 物品类型id
	amount   = 1,			%% 要创建多少个
	is_bind  = default,		%% 是否要绑定(bool)，如果不关心，则执行默认值
	lifetime = default,		%% 使用寿命(单位:秒)(integer)，如果不关心，则执行默认值
	firm_lv  = default,		%% 强化等级(integer)，如果不关心，则执行默认值
	colour   = default,		%% 颜色品质(integer)，如果不关心，则执行默认值
	attr     = default,		%% 加成属性(integer)，如果不关心，则执行默认值
	log 					%% 道具创建的日志记录
}).

%% 物品使用要求record
-record (c_use_requirement, {
	min_lv = 0 		%% 使用物品的最低等级要求
}).

%% 物品使用的效果record
-record (c_use_effect, {
	func = 0, 			%% 物品使用的回调方法名称
	args = undefined 	%% 传递给回调方法的参数(term())
}).

%% 物品配置
-record (c_item, {
	type_id     = 0,					%% 物品的类型id
	name        = "",					%% 物品名称
	kind        = ?ITEM_KIND_MATERIAL,	%% 物品类别
	colour      = ?ITEM_COLOUR_WHITE,	%% 物品颜色
	is_overlap  = false,				%% 是否可以堆叠
	location        = 0,				%% 装备类物品的装备位置(参见宏：ITEM_LOCATION_XXX)
	endurance   = ?ENDURANCE_NEVER,		%% 耐久度
	requirement = undefined,			%% 使用要求(undefined|#c_use_requirement{})
	lifetime    = ?LIFETIME_ENDLESS,	%% 物品寿命
	is_bind     = true,					%% 是否绑定
	firm_lv     = 0,					%% 给予的初始强化等级
	attr        = [],					%% 给予的天赋属性
	use_effect  = undefined				%% 使用效果
}).