-module(conn_sup_tests).

-include_lib("eunit/include/eunit.hrl").

create_test() ->
  recover:start(),
  crypto:start(),
  ssh:start(),
  conn_sup:start_link(responder_mock),
  ?assertMatch([], supervisor:which_children(conn_sup)),

  conn_sup:start_child_if_not_exist("localhost", 
    fun(_Host) ->
      [{user,"lifu"}, {password,"hello1234"}, {port,22}]
    end),
  ?assertMatch([{undefined, _Pid, worker, [client]}], supervisor:which_children(conn_sup)),
  client:do_cmd("localhost"),
  timer:sleep(300),
  recover:stop(),
  ok.

