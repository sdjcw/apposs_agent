%% 创建新的进程来执行指令，并返回新进程的pid，可以通过该pid强制终止指令的执行。
-module(ssh_executor).
-export([conn_manager/2, exec/2, handle_info/3, terminate/1]).

-include("agent.hrl").
-define(timeout, 10000).

-ifdef(TEST).
-compile(export_all).
-endif.

conn_manager(Host, Options) ->
  [{opts,Opts},{port,Port}] = handle_options(Options),
  ssh:connect(Host, Port, add_def_opts(Opts), ?timeout).

handle_options(Opts) ->
  handle_options(proplists:unfold(Opts), [], 22).

handle_options([], Opts, Port) ->
  [{opts, Opts},{port, Port}];
handle_options([{port, Port} | Rest], Opts, Port) ->
  P = if 
    Port =:= "" -> 22;
    is_list(Port) -> list_to_integer(Port);
    is_integer(Port) -> Port
  end,
  handle_options(Rest, Opts, P);
handle_options([{user, _} = Opt | Rest], Opts, Port) ->
  handle_options(Rest, [Opt | Opts], Port);
handle_options([{password, _} = Opt | Rest], Opts, Port) ->
  handle_options(Rest, [Opt | Opts], Port);
handle_options([_Opt | Rest], Opts, Port) ->
  handle_options(Rest, Opts, Port).

add_def_opts(Opts) ->
  add_def_opts(Opts, [{silently_accept_hosts, true}, 
                      {user_interaction, false}]).

add_def_opts(Opts, []) ->
  Opts;
add_def_opts(Opts, [{K, _} = DefOpt | Rest]) ->
  case proplists:is_defined(K, Opts) of
    false -> add_def_opts([DefOpt | Opts], Rest);
    true -> add_def_opts(Opts, Rest)
  end.

exec(Conn, {Command, _Oid}) ->
  {ok, Chl} = ssh_connection:session_channel(Conn,?timeout),
  success = ssh_connection:exec(Conn,Chl,Command,?timeout),
  Chl.

handle_info({ssh_cm, Conn, {data, Chl, Type_code, Data}}, Conn, Chl) ->
  D = binary_to_list(Data), 
  ?INFO("ssh_cm ~p [~p]: ~ts~n", [Chl, Type_code, D]),
  {data, D};
handle_info({ssh_cm, Conn, {exit_status, Chl, ExitStatus}}, Conn, Chl) ->
  ?INFO("session ~p exit: ~p.~n",[Chl, ExitStatus]),
  {exit_status, ExitStatus};
handle_info({ssh_cm, Conn, {eof,Chl}}, Conn, Chl) ->
  ?INFO("ssh_cm ~p eof.~n", [Chl]),
  eof;
handle_info({ssh_cm, Conn, {exit_signal, Chl, ExitSignal, ErrorMsg, LanguageString}}, Conn, Chl) ->
  ?WARN("ssh_cm ~p exit_signal: ~p, ~p, ~p~n", [Chl, ExitSignal, ErrorMsg, LanguageString]),
  Reason = lists:append(["Apposs added: exit_signal: ", ExitSignal, ", ErrorMsg: ", ErrorMsg, ", LanguageString: ", LanguageString, ". Maybe user interrupt."]),
  {exit_signal, Reason};
handle_info({ssh_cm, Conn, {closed,Chl}}, Conn, Chl) ->
  ?INFO("ssh_cm ~p closed.~n", [Chl]),
  closed. 

terminate(Conn) ->
  ?INFO("ssh close.~n"),
  ssh:close(Conn).
