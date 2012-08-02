-module(recover_tests).

-include_lib("eunit/include/eunit.hrl").

create_test() ->
  ?assertMatch(ok, recover:start()),
  ?assertMatch(def_v1, recover:recover(k1, def_v1)),
  recover:save(k1, v1),
  ?assertMatch(v1, recover:recover(k1, def_v1)),
  ?assertMatch(def_v1, recover:recover(k1, def_v1)),
  ?assertMatch(ok, recover:stop()).



