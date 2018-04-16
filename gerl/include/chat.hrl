%% @doc 聊天服务代码相关的头文件

%% ============================================================
%% 注意添加了频道名称后需要添加对应的频道id，
%% 并要在util_chat.erl中添加对应的映射方法
%% 聊天频道名称定义
-define(CHANNEL_PRIVATE, 	channel_private).	%% 私人聊天频道
-define(CHANNEL_TEAM, 		channel_team).		%% 队伍聊天频道
-define(CHANNEL_WORLD, 		channel_world).		%% 世界聊天频道

%% 聊天频道ID定义
-define(CHANNEL_ID_PRIVATE, 	0).		%% 私人聊天
-define(CHANNEL_ID_TEAM, 		1).		%% 队伍聊天
-define(CHANNEL_ID_WORLD, 		2).		%% 世界聊天
%% ============================================================

%% 聊天返回给客户端的code定义
-define(CHAT_CODE_OK,			0).		%% 成功
-define(CHAT_CODE_NO_PLAYER,	1).		%% 无此玩家
-define(CHAT_CODE_OFFLINE,		2).		%% 不在线
-define(CHAT_CODE_NO_SPEAK,		3).		%% 当前处于禁言状态

%% 聊天室信息record
-record (r_chat_room, {
	room_key,		%% 聊天室的索引key
	members = []	%% 里面的成员
}).

%% 聊天实体record
-record (r_chat_entity, {
	id       = 0,		%% 实体id
	name     = "",		%% 实体的名称
	level    = 0,		%% 实体的等级
	no_speak = 0		%% 禁言时间，表示到何时才解除禁言，-1表示永久禁言
}).
