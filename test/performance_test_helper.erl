-module(performance_test_helper).

-export([add_conn/1]).

add_conn(0) ->
  ok;
add_conn(N) ->
  Host = "h" ++ integer_to_list(N),
  case client:check_host(Host) of
    no_host ->
      conn_sup:start_child([{host,Host},{user,"lifu"},{password,"hello1234"},{port,9999}]),
      dispatcher:dispatch(Host, "hold:f(3000).", N),
      timer:sleep(100),
      add_conn(N - 1);
    ok ->
      ok
  end.
  

