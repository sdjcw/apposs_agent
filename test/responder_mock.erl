-module(responder_mock).

-compile(export_all).

run_caller(_Profile) ->
  fun(Host, Cmd) -> 
    error_logger:info_msg("~p:run_caller(), ~p, ~p~n", [?MODULE, Host, Cmd])
  end.

cb_caller(_Profile) ->
  fun(Host, Cmd, {Isok, Body}) -> 
    error_logger:info_msg("~p:cb_caller(), ~p, ~p, {~p, ~ts}~n", [?MODULE, Host, Cmd, Isok, Body])
  end.

machine_on_caller(_Profile) ->
  fun(Host, Event) ->
    error_logger:info_msg("~p:machine_on_caller(), ~p, ~p~n", [?MODULE, Host, Event])
  end.
