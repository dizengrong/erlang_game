%% @doc npc配置

-module (cfg_npc).
-include ("fight.hrl").
-include ("npc.hrl").
-compile([export_all]).

get(10000) -> 
	#c_npc{
		type_id      = 10000, 
		move_radius  = 10,
		guard_radius = 8,
		move_speed   = 100,
		attr = #r_fight_attr{
			attack     = 10,
			defence    = 10,
			initiative = 10,
			speed	   = 10,
			crit       = 10,
			damage     = 20,
			hp         = 100,
			max_hp     = 100
		}
	};
get(10001) -> 
	#c_npc{
		type_id      = 10001, 
		move_radius  = 10,
		guard_radius = 8,
		move_speed   = 100,
		attr = #r_fight_attr{
			attack     = 10,
			defence    = 10,
			initiative = 10,
			speed	   = 10,
			crit       = 10,
			damage     = 20,
			hp         = 100,
			max_hp     = 100
		}
	};
get(NpcTypeId) -> erlang:throw({?MODULE, error_confg, 'get/1', NpcTypeId}).
