-ifndef(MS_MAP_ENTER_PB_H).
-define(MS_MAP_ENTER_PB_H, true).
-record(ms_map_enter, {
    map_id
}).
-endif.

-ifndef(MC_MAP_ENTER_PB_H).
-define(MC_MAP_ENTER_PB_H, true).
-record(mc_map_enter, {
    roles = [],
    npcs = []
}).
-endif.

-ifndef(MC_MAP_LEAVE_PB_H).
-define(MC_MAP_LEAVE_PB_H, true).
-record(mc_map_leave, {
    roles = [],
    npcs = []
}).
-endif.

-ifndef(MS_MAP_JUMP_PB_H).
-define(MS_MAP_JUMP_PB_H, true).
-record(ms_map_jump, {
    dest_map_id
}).
-endif.

-ifndef(MC_MAP_JUMP_PB_H).
-define(MC_MAP_JUMP_PB_H, true).
-record(mc_map_jump, {
    dest_map_id,
    dest_x,
    dest_y
}).
-endif.

-ifndef(MS_MAP_MOVE_PB_H).
-define(MS_MAP_MOVE_PB_H, true).
-record(ms_map_move, {
    path = []
}).
-endif.

-ifndef(MC_MAP_MOVE_PB_H).
-define(MC_MAP_MOVE_PB_H, true).
-record(mc_map_move, {
    object_type,
    object_id,
    path = []
}).
-endif.

-ifndef(MS_MAP_MOVE_CHECK_PB_H).
-define(MS_MAP_MOVE_CHECK_PB_H, true).
-record(ms_map_move_check, {
    x,
    y
}).
-endif.

