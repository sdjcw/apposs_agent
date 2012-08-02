-module(responder_tests).
-include_lib("eunit/include/eunit.hrl").

run_caller_test() ->
  simple_http_server:start(3333),
  inets:start(),
  http_channel_sup:start_link(),
  http_channel_sup:start_child(profile_responder),
  responder:start_link("http://localhost:3333"),
  RunCaller = responder:run_caller(profile_responder),
  ?assertMatch({ok, ""}, RunCaller("localhost", {"echo hello", 1})),
  
  CbCaller = responder:cb_caller(profile_responder),
  ?assertMatch({ok, ""}, CbCaller("localhost", {"echo hello", 1},{true, "hello\n"})),
  inets:stop(),
  simple_http_server:stop().
