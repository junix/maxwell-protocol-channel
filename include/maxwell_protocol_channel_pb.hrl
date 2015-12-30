-ifndef(CHAN_AGENT_ID_T_PB_H).
-define(CHAN_AGENT_ID_T_PB_H, true).
-record(chan_agent_id_t, {
    user_id = erlang:error({required, user_id}),
    agent_key = erlang:error({required, agent_key})
}).
-endif.

-ifndef(CHAN_SESSION_ID_T_PB_H).
-define(CHAN_SESSION_ID_T_PB_H, true).
-record(chan_session_id_t, {
    user_id = erlang:error({required, user_id}),
    session_key = erlang:error({required, session_key})
}).
-endif.

-ifndef(CHAN_COMMAND_T_PB_H).
-define(CHAN_COMMAND_T_PB_H, true).
-record(chan_command_t, {
    dest_type = erlang:error({required, dest_type}),
    dest_id = erlang:error({required, dest_id}),
    payload = erlang:error({required, payload}),
    src_type,
    src_id
}).
-endif.

