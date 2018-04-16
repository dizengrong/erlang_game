%% @doc 战斗相关定义的头文件

-record (r_fight_attr, {
	attack     = 0,		%% 攻击力
	defence    = 0,		%% 防御力
	intel      = 0,		%% 智力
	initiative = 0,		%% 主动性
	speed	   = 0,		%% 行动力
	crit       = 0,		%% 致命一击(万分比)
	damage     = 0,		%% 伤害
	hp         = 0,		%% 生命
	max_hp     = 0		%% 最大生命
}).