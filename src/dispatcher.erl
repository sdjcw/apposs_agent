-module(dispatcher).
-export([dispatch/3]).

dispatch(Host,"machine|pause"=Cmd,Oid) ->
  client:pause(Host),
  (responder:cb_caller(poller))(Host, {Cmd, Oid},{true, "done"});
dispatch(Host,"machine|reset"=Cmd,Oid) ->
  client:reset(Host),
  (responder:cb_caller(poller))(Host, {Cmd, Oid},{true, "done"});
dispatch(Host,"machine|interrupt"=Cmd,Oid) ->
  client:interrupt(Host),
  (responder:cb_caller(poller))(Host, {Cmd, Oid},{true, "done"});
dispatch(Host, "machine|reconnect"=Cmd, Oid) ->
  client:reconnect(Host),
  (responder:cb_caller(poller))(Host, {Cmd, Oid},{true, "done"});
dispatch(Host,"machine|clean_all",Oid) ->
  client:clean_cmds(Host),
  (responder:cb_caller(poller))(Host, {"machine|clean_all", Oid},{true, "done"});
dispatch(Host,Cmd,Oid) ->
  client:add_cmd(Host, {Cmd, Oid}).

