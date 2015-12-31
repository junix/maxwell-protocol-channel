-ifndef(CHAN_HEARTBEAT_T_PB_H).
-define(CHAN_HEARTBEAT_T_PB_H, true).
-record(chan_heartbeat_t, {
    
}).
-endif.

-ifndef(CHAN_MSG_T_PB_H).
-define(CHAN_MSG_T_PB_H, true).
-record(chan_msg_t, {
    rpc_type,
    msg_type,
    payload = erlang:error({required, payload})
}).
-endif.

