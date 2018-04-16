-ifndef(MS_LOGIN_PB_H).
-define(MS_LOGIN_PB_H, true).
-record(ms_login, {
    account_name
}).
-endif.

-ifndef(MC_LOGIN_PB_H).
-define(MC_LOGIN_PB_H, true).
-record(mc_login, {
    ret_code = 0,
    role_id
}).
-endif.

-ifndef(MS_REGISTER_PB_H).
-define(MS_REGISTER_PB_H, true).
-record(ms_register, {
    role_name,
    hero_id
}).
-endif.

-ifndef(MC_REGISTER_PB_H).
-define(MC_REGISTER_PB_H, true).
-record(mc_register, {
    ret_code = 0,
    role_id
}).
-endif.

-ifndef(MS_ENTER_GAME_PB_H).
-define(MS_ENTER_GAME_PB_H, true).
-record(ms_enter_game, {
    role_id
}).
-endif.

-ifndef(MC_ENTER_GAME_PB_H).
-define(MC_ENTER_GAME_PB_H, true).
-record(mc_enter_game, {
    role_id,
    map_id,
    x,
    y
}).
-endif.

