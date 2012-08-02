-module(ssh_test_helper).

-export([s_host/3, m_host/2]).

get_params() ->
  dict:from_list([{port,9999},{user,"lifu"},{password, "hello1234"}]).

s_host(N, Host, Delay) ->
  Params = dict:store(host, Host, get_params()),
  f(N, Params, Delay).

f(0, _Params, _Delay) ->
  ok;
f(N, Params, Delay) ->
  ssh_executor:async_do_cmd(Params, {"receive ok -> ok end.", 1}, fun cb_f/1),
  timer:sleep(Delay),
% ssh_executor:do_cmd(Params, "1 + 1.", fun cb_f/1),
  f(N - 1, Params, Delay).

m_host(N, Delay) ->
  Params = dict:from_list([{port,9999},{user,"lifu"},{password, "hello1234"}]),
  P1 = dict:store(host, "10.232.37.143", Params),
  P2 = dict:store(host, "10.232.37.141", Params),
  P3 = dict:store(host, "10.13.121.70", Params),
  m_host(N, [P2, P3, P1], Delay).

m_host(0, _Ps, _Delay) ->
  ok;
m_host(N, Ps, Delay) ->
  lists:foreach(fun m_host/1, Ps),
  timer:sleep(Delay),
  m_host(N - 1, Ps, Delay).

m_host(Params) ->
  ssh_executor:async_do_cmd(Params, {"receive ok -> ok end.", 1}, fun
    ({true, _}) ->
      ok;
    ({false, Why}) ->
      error_logger:error_msg("########################~n~p, ~p~n########################~n", [dict:find(host, Params), Why])
  end).

cb_f({true, _}) ->
  ok;
cb_f({false, Why}) ->
  error_logger:error_msg("########################~n~p~n########################~n", [Why]).

