-module(event_scenario).
-export([start/0, add_handler/2, send_msg/2, main/0, shutdown/1, remove_id/2]).

start()->
    gen_event:start().

add_handler(Pid, Module)->
    HandlerId = {Module, make_ref()},
    gen_event:add_handler(Pid,HandlerId, []),
    HandlerId.

send_msg(Pid, Msg)->
    gen_event:notify(Pid, Msg).

remove_id(Pid, ID)->
    gen_event:delete_handler(Pid, ID, []).

shutdown(Pid)->
    gen_event:stop(Pid).

main()->
    {ok, Pid} = start(),
    ID = add_handler(Pid, events),
    _ID2 = add_handler(Pid, events2),
    send_msg(Pid, test),
    gen_event:call(Pid, ID, only_for_event1),
    remove_id(Pid, ID),
    send_msg(Pid, only_for_event2),
    timer:sleep(300),
    shutdown(Pid).
