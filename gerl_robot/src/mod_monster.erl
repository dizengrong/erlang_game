-module (mod_monster).
-compile([export_all]).
-include ("common.hrl").

-define(TAB_MONSTERS, erlang:get(tab_monsters)).

init() ->
	ok.
	% TabId = ets:new(tab_monsters, [ordered_set, public, {keypos, #p_map_monster.monsterid}]),
	% erlang:put(tab_monsters, TabId).
