-module(ssh_executor_tests).
-include_lib("eunit/include/eunit.hrl").

conn_manager_test() ->
  crypto:start(),
  ssh:start(),
  {ok, Cm} = ssh_executor:conn_manager("localhost", get_opts()),
  ssh_executor:terminate(Cm).

conn_manager_no_host_test() ->
  crypto:start(),
  ssh:start(),
  ?assertMatch({error,nxdomain}, ssh_executor:conn_manager("noThisHost", get_opts())).

conn_manager_error_login_test() ->
  crypto:start(),
  ssh:start(),
  ?assertMatch({error, _}, ssh_executor:conn_manager("localhost", [{user, "noThisUser"}, {password, "noThisPassword"}])).

conn_manager_empty_login_test() ->
  crypto:start(),
  ssh:start(),
  ?assertMatch({error, _}, ssh_executor:conn_manager("localhost", [])).

exec_test() ->
  crypto:start(),
  ssh:start(),
  {ok, Cm} = ssh_executor:conn_manager("localhost", get_opts()),
  H1 = ssh_executor:exec(Cm, {"echo hello", 1}),
  ?assertMatch({true, "hello\n"}, receive_loop(Cm, H1)),

  H2 = ssh_executor:exec(Cm, {"echo world", 2}),
  ?assertMatch({true, "world\n"}, receive_loop(Cm, H2)),
  ssh_executor:terminate(Cm).

receive_loop(Cm, Handler) ->
  receive_loop(Cm, Handler, [], -1).

receive_loop(Cm, Handler, Datas, ExitStatus) ->
  receive
    Info ->
      case ssh_executor:handle_info(Info, Cm, Handler) of
        {data, Data} ->
          receive_loop(Cm, Handler, [Data | Datas], ExitStatus);
        {exit_status, Es} ->
          receive_loop(Cm, Handler, Datas, Es);
        eof ->
          receive_loop(Cm, Handler, Datas, ExitStatus);
        closed ->
          Ds = lists:append(lists:reverse(Datas)),
          case ExitStatus of
            0 -> {true, Ds};
            _ -> {false, Ds}
          end
      end
  end.

handle_options_test_() ->
  [
   ?_assertMatch([{port, 22}], ssh_executor:handle_options([{port, ""}])),
   ?_assertMatch([{port, 222}], ssh_executor:handle_options([{port, "222"}])),
   ?_assertMatch([{port, 333}], ssh_executor:handle_options([{port, 333}])),
   ?_assertMatch([{password, "hello1234"}, {user,"lifu"}],
                 ssh_executor:handle_options([{user,"lifu"},
                                              {password, "hello1234"}, 
                                              {foo, "bar"}])),
   ?_assertMatch([], ssh_executor:handle_options([]))
  ].

add_def_opts_test_() ->
  [
   ?_assertMatch([{port, 22},
                  {user_interaction, false}, 
                  {silently_accept_hosts, true}], 
                  ssh_executor:add_def_opts([]))
  ].

get_opts() ->
  [{user,"lifu"},{password, "hello1234"}].

