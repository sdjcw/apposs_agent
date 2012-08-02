-module(poller_tests).
-include_lib("eunit/include/eunit.hrl").

analyse_cmds_test_() ->
  [?_assertMatch([{"localhost", "stop", "1"},
                  {"localhost", "redeploy", "2"},
                  {"test", "start", "3"}],
                 poller:analyse_cmds("localhost:stop:1\nlocalhost:redeploy:2\ntest:start:3")),
   ?_assertMatch([{"localhost", "Aa123 :;~!@#$%^&*()\"\"''", "1"}],
                 poller:analyse_cmds("localhost:Aa123 :;~!@#$%^&*()\"\"'':1")),
   ?_assertMatch([], poller:analyse_cmds("")),
   ?_assertMatch([], poller:analyse_cmds("ok"))
  ].

