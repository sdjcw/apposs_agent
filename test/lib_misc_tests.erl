-module(lib_misc_tests).
-include_lib("eunit/include/eunit.hrl").

invoke_do_my_best_test() ->
  F = fun() ->
    {error, "invoke_do_my_best_test function test."}
  end,
  lib_misc:invoke_do_my_best(F, 1, 3, 100),
  
  F2 = fun() ->
    ok
  end,
  lib_misc:invoke_do_my_best(F2, 1, 1000, 10000).

parse_string_test_() ->
  [
    ?_assertEqual(lib_misc:parse_string("a=b,c=d,x=y"), [{a,"b"},{c,"d"},{x,"y"}]),
    ?_assertEqual(lib_misc:parse_string("a=b,c=d,x=y\n"), [{a,"b"},{c,"d"},{x,"y\n"}])
  ].

join_params_test() ->
  Params = [{a, 1}, {b, "hello"}, {c, "中文\nab=11&bc=22\n1 + 2 = 3\n"}],
  ?assertEqual(
    "a=1&b=%68%65%6C%6C%6F&c=%E4%B8%AD%E6%96%87%0A%61%62%3D%31%31%26%62%63%3D%32%32%0A%31%20%2B%20%32%20%3D%20%33%0A", 
    lib_misc:join_params(Params)).

string_to_utf8_test_() ->
  [
   ?_assertEqual("%E4%B8%AD%E6%96%87", lib_misc:string_to_utf8("中文"))
  ].

list_to_hex_test_() ->
  [?_assertEqual([], lib_misc:list_to_hex([])),
   ?_assertEqual(["00"], lib_misc:list_to_hex([0])),
   ?_assertEqual(["01","10","FF","67"], lib_misc:list_to_hex([1, 16, 255, 103]))].
