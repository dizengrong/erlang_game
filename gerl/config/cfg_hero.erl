%% @doc 英雄配置

-module (cfg_hero).
-include("hero.hrl").
-compile([export_all]).

-spec get_hero(HeroId::integer()) -> #c_hero{}.
%% 初始的英雄配置
get_hero(1) -> #c_hero{
	hero_id    = 1,
	category   = ?CATEGORY_WARRIOR,
	leadership = 1000,
	attack     = 10,
	defence    = 8,
	intel      = 4,
	max_rage   = 30,
	max_magic  = 20
};
get_hero(2) -> #c_hero{
	hero_id    = 3,
	category   = ?CATEGORY_KNIGHT,
	leadership = 1000,
	attack     = 8,
	defence    = 6,
	intel      = 5,
	max_rage   = 25,
	max_magic  = 25
};
get_hero(3) -> #c_hero{
	hero_id    = 3,
	category   = ?CATEGORY_MAGIC,
	leadership = 1000,
	attack     = 4,
	defence    = 5,
	intel      = 10,
	max_rage   = 20,
	max_magic  = 30
};
get_hero(HeroId) -> erlang:throw({?MODULE, error_confg, 'get_hero/1', HeroId}).

-spec get_troops(HeroId::integer()) -> [{TroopsId::integer(), 		%% 军队类型id
										 TroopsAmount::integer(), 	%% 军队数量
										 TroopsPlace::integer(), 	%% 驻军地点
										 TroopsLevel::integer()		%% 军队等级
										}].
%% 初始的军队
get_troops(1) -> [{1, 10, 1, 1}, {2, 10, 2, 1}, {3, 10, 3, 1}];
get_troops(2) -> [{1, 10, 1, 1}, {2, 10, 2, 1}, {3, 10, 3, 1}];
get_troops(3) -> [{1, 10, 1, 1}, {2, 10, 2, 1}, {3, 10, 3, 1}];
get_troops(HeroId) -> erlang:throw({?MODULE, error_confg, 'get_troops/1', HeroId}).
