-record(robot, {
	role_id      = 0,
	account      = "",
	role_name    = "",
	status       = 0,
	sokcet       = undefined,
	category     = 0, 			%% 职业
	ai           = 0, 			%% robot的ai类型
	ai_datas     = undefined,  	%% ai数据
	map_id       = 0,			%% 当前所在地图id
	pos          = undefined,	%% #p_pos{} 当前坐标
	walk_path 	 = [], 			%% 走路的路径
	level        = 0, 			%% 等级
	silver 		 = 0, 			%% 铜币
	gold 		 = 0, 			%% 元宝
	skills 		 = [],			%% 技能数据[#p_role_skill{}]
	jingjie   	 = 0, 			%% 境界
	move_speed   = 0 			%% 移动速度
}).

%% robot的AI定义
-define(AI_TYPE_ATTACK_MONSTER, 		1).	 %% 攻击怪物的ai
%% 全部ai的列表
-define(ALL_AI_TYPES, [?AI_TYPE_ATTACK_MONSTER]).

-define(ROBOT_STATUS_IDLE, 				0).  %% 处于空闲状态
-define(ROBOT_STATUS_LOGIN, 			1).  %% 处于登陆状态
-define(ROBOT_STATUS_CHOOSE_ROLE, 		2).  %% 处于登陆选角色状态
-define(ROBOT_STATUS_REGISTER_ROLE, 	3).  %% 处于登陆注册新角色状态
-define(ROBOT_STATUS_ENTER_GAME, 		4).  %% 处于登陆注册新角色状态
-define(ROBOT_STATUS_ENTER_MAP, 		5).  %% 处于进入地图状态
-define(ROBOT_STATUS_WAITING, 			6).  %% 处于等待下一个消息的状态
-define(ROBOT_STATUS_WALKING, 			7).  %% 处于走路状态
-define(ROBOT_STATUS_DEAD, 				8).  %% 处于死亡状态
-define(ROBOT_STATUS_CAST_LAST_SKILL, 	9).  %% 处于施放持续性技能状态


-define(_if(IF, Expr1, Expr2), case IF of true -> Expr1; false -> Expr2 end).

-define(TYPE_ROLE, 		1). %% 战斗的目标类型:人
-define(TYPE_MONSTER, 	2). %% 战斗的目标类型:怪物
