%% 职业定义
-define(CATEGORY_WARRIOR, 	1).	%% 战士职业
-define(CATEGORY_KNIGHT, 	2).	%% 骑士职业
-define(CATEGORY_MAGIC, 	3).	%% 法师职业

%% 英雄配置record
-record (c_hero, {
	hero_id    = 0,		%% 等级
	category   = 0,		%% 职业
	leadership = 0,		%% 领导力
	attack     = 0,		%% 攻击力
	defence    = 0,		%% 防御力
	intel      = 0,		%% 智力
	max_rage   = 0,		%% 最大狂怒值
	max_magic  = 0		%% 最大魔法值
}).


