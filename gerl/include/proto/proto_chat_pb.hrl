-ifndef(MS_CHAT_PB_H).
-define(MS_CHAT_PB_H, true).
-record(ms_chat, {
    channel,
    receiver,
    content
}).
-endif.

-ifndef(MC_CHAT_PB_H).
-define(MC_CHAT_PB_H, true).
-record(mc_chat, {
    ret_code = 0,
    channel,
    content,
    sender
}).
-endif.

