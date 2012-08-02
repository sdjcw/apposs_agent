-module(webutil_tests).
-include_lib("eunit/include/eunit.hrl").

web_test() ->
  inets:start(),
  webutil:start_link(default),
  ?assertMatch({ok, _Result}, webutil:http_get(default, "http://www.baidu.com")).
