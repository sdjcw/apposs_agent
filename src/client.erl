-module(client).
-behaviour(gen_fsm).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/3, start_link/3, stop/1, reconnect/1, check_host/1, add_cmd/2, do_cmd/1, todo_cmds/1, clean_cmds/1, pause/1, interrupt/1, reset/1, get_state/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, disconnected/2, normal/2, run/2, paused/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

-ifdef(TEST).
-compile(export_all).
-endif.

-define(SERVER(Host), list_to_atom(Host ++ "@" ++ atom_to_list(?MODULE))).
-record(state, {host,
                get_host_info_fun,          % 获取主机信息的方法
                conn_params,                % 链接参数
                cm,                         % connectionManager
                handler,                    % 链接通道的句柄
                current_cmd,                % 当前正在执行的指令 
                datas=[],                   % 当前指令返回的信息，包含标准输出和标准错误
                cmd_exit_status,            % 当前指令的返回码
                exec_mod=ssh_executor,      % 指令执行器
                responder_mod=responder,    % 指令回报模块
                cmds=[]                     % 未执行的指令队列
               }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Responder_mod, Host, GetHostInfoFun) ->
  gen_fsm:start({local, ?SERVER(Host)}, ?MODULE, [Responder_mod, Host, GetHostInfoFun], []).

start_link(Responder_mod, Host, GetHostInfoFun) ->
  gen_fsm:start_link({local, ?SERVER(Host)}, ?MODULE, [Responder_mod, Host, GetHostInfoFun], []).

stop(Host) ->
  gen_fsm:sync_send_all_state_event(?SERVER(Host), stop).

check_host(Host) ->
  case erlang:whereis(?SERVER(Host)) of
    undefined -> no_host;
    _Pid -> ok
  end.

reconnect(Host) ->
  interrupt(Host),
  gen_fsm:send_all_state_event(?SERVER(Host), reconnect),
  reset(Host).

add_cmd(Host, Cmd) ->
  gen_fsm:send_all_state_event(?SERVER(Host), {add_cmd, Cmd}).

do_cmd(Host) ->
  gen_fsm:send_event(?SERVER(Host), do_cmd).

todo_cmds(Host) ->
  gen_fsm:sync_send_all_state_event(?SERVER(Host), todo_cmds).

clean_cmds(Host) ->
  gen_fsm:send_all_state_event(?SERVER(Host), clean_cmds).

pause(Host) ->
  gen_fsm:send_event(?SERVER(Host), pause).

interrupt(Host) ->
  gen_fsm:send_event(?SERVER(Host), interrupt).

reset(Host) ->
  gen_fsm:send_event(?SERVER(Host), reset).

get_state(Host) ->
  gen_fsm:sync_send_all_state_event(?SERVER(Host), get_state).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([Responder_mod, Host, GetHostInfoFun]) ->
  Cmds = recover:recover(?SERVER(Host), []),
  State = #state{host = Host,
                 get_host_info_fun=GetHostInfoFun,
                 responder_mod = Responder_mod,
                 cmds = Cmds
                },
  error_logger:info_msg("machine init: ~p~n", [Host]),
  gen_fsm:send_all_state_event(?SERVER(Host), reconnect),
  {ok, disconnected, State}.

disconnected(Event, #state{host=Host}=State) ->
  error_logger:info_msg("machine [~p] disconnected, ignore ~p~n", [Host, Event]),
  {next_state, disconnected, State}.

normal(do_cmd, #state{host=Host, cmds=[]}=State) ->
  error_logger:info_msg("machine [~p] do_cmd, normal -> normal, cmds=[]~n", [Host]),
  {next_state, normal, State};
normal(do_cmd, #state{host=Host, cm=Cm, cmds=[Cmd|T_cmds], exec_mod=ExecMod, responder_mod=RespMod}=State) ->
  error_logger:info_msg("machine [~p] do_cmd, normal -> run, CurrentCmd=~p, Cmds=~p~n", [Host, Cmd, T_cmds]),
  (RespMod:run_caller(client))(Host, Cmd),
  Handler = ExecMod:exec(Cm, Cmd),
  {next_state, run, State#state{current_cmd=Cmd, cmds=T_cmds, handler=Handler}};
normal(pause, #state{host=Host, responder_mod=RespMod}=State) ->
  error_logger:info_msg("machine [~p] pause, normal -> paused.~n", [Host]),
  (RespMod:machine_on_caller(client))(Host, pause),
  {next_state, paused, State};
normal(interrupt, #state{host=Host, responder_mod=RespMod}=State) ->
  error_logger:info_msg("machine [~p] interrupt, normal -> paused~n", [Host]),
  (RespMod:machine_on_caller(client))(Host, pause),
  {next_state, paused, State};
normal(Event, #state{host=Host}=State) ->
  error_logger:info_msg("machine [~p] is normal state, ignore ~p~n", [Host, Event]),
  {next_state, normal, State}.

run(interrupt, #state{host=Host, cm=Cm, current_cmd=Cmd, exec_mod=ExecMod, datas=Datas, conn_params=ConnParams, responder_mod=RespMod}=State) ->
  error_logger:info_msg("machine [~p] interrupt, current_cmd=~p, run -> paused.~n", [Host, Cmd]),
  ExecMod:terminate(Cm),
  {ok, NewCm} = ExecMod:conn_manager(Host, ConnParams),
  {paused, TempState} = finish_cmd(run, State#state{cmd_exit_status=1, datas=["User interrupt." | Datas]}),
  (RespMod:machine_on_caller(client))(Host, pause),
  {next_state, paused, TempState#state{cm=NewCm}};
run(pause, #state{host=Host, responder_mod=RespMod}=State) ->
  error_logger:info_msg("machine [~p] pause, run -> paused.~n", [Host]),
  (RespMod:machine_on_caller(client))(Host, pause),
  {next_state, paused, State};
run(Event, #state{host=Host}=State) ->
  error_logger:info_msg("machine [~p] is run state, ignore ~p~n", [Host, Event]),
  {next_state, run, State}.

paused(reset, #state{host=Host, current_cmd=undefined, responder_mod=RespMod}=State) ->
  error_logger:info_msg("machine [~p] reset, paused -> normal.~n", [Host]),
  do_cmd(Host),
  (RespMod:machine_on_caller(client))(Host, reset),
  {next_state, normal, State};
% 有可能先运行了耗时长的指令，然后pause，然后马上reset，此时先前的命令还未执行结束，所以应该直接进入run状态
paused(reset, #state{host=Host, current_cmd=Cmd, responder_mod=RespMod}=State) ->
  error_logger:info_msg("machine [~p] reset, current_cmd=~p, paused -> run.~n", [Host, Cmd]),
  (RespMod:machine_on_caller(client))(Host, reset),
  {next_state, run, State};
paused(interrupt, #state{host=Host, cm=Cm, current_cmd=Cmd, exec_mod=ExecMod, datas=Datas, conn_params=ConnParams, responder_mod=RespMod}=State) when Cmd /= undefined ->
  error_logger:info_msg("machine [~p] interrupt, current_cmd=~p, paused -> paused.~n", [Host, Cmd]),
  ExecMod:terminate(Cm),
  {ok, NewCm} = ExecMod:conn_manager(Host, ConnParams),
  {paused, TempState} = finish_cmd(run, State#state{cmd_exit_status=1, datas=["User interrupt." | Datas]}),
  (RespMod:machine_on_caller(client))(Host, pause),
  {next_state, paused, TempState#state{cm=NewCm}};
paused(Event, #state{host=Host}=State) ->
  error_logger:info_msg("machine [~p] is paused state, ignore ~p~n", [Host, Event]),
  {next_state, paused, State}.

%state_name(_Event, _From, State) ->
%    {reply, ok, state_name, State}.

handle_event({add_cmd, Cmd}, StateName, #state{host=Host, current_cmd=CurrentCmd, cmds=Cmds}=State) ->
  case lists:member(Cmd, Cmds) orelse CurrentCmd =:= Cmd of
    true -> 
      {next_state, StateName, State};
    false ->
      New_all_cmds = lists:append(Cmds, [Cmd]),
      error_logger:info_msg("machine [~p] add_cmd, cmd=~p, all=~p~n", [Host, Cmd, New_all_cmds]),
      do_cmd(Host),
      {next_state, StateName, State#state{cmds=New_all_cmds}}
  end;
handle_event(clean_cmds, StateName, #state{host=Host, cmds=Cmds}=State) ->
  error_logger:info_msg("machine [~p] clean_cmds, discard=~p", [Host, Cmds]),
  {next_state, StateName, State#state{cmds=[]}};
handle_event(reconnect, _StateName, #state{host=Host, exec_mod=ExecMod, responder_mod=RespMod}=State) ->
  error_logger:info_msg("machine [~p] connect...~n", [Host]),
  case State#state.cm of
    undefined -> ok;
    OldCm -> ExecMod:terminate(OldCm)
  end,
  case create_connection_manager(Host, State#state.get_host_info_fun) of
    {ok, Cm, ExecMod, Params} ->
      NextState = case proplists:get_value(state, Params) of
        undefined -> 
          normal;
        "disconnected" -> 
          % 如果machine当前为disconnected state,
          (RespMod:machine_on_caller(client))(Host, reset),
          do_cmd(Host),
          normal;
        InnerState -> 
          list_to_atom(InnerState)
      end,
      error_logger:info_msg("machine [~p] state: ~p - ~p~n", [Host, State, NextState]),
      {next_state, NextState, State#state{cm=Cm, conn_params=Params, exec_mod=ExecMod}};
    {error, Why} ->
      error_logger:error_msg("machine [~p] connect error: ~p~n", [Host, Why]),
      (RespMod:machine_on_caller(client))(Host, disconnect),
      {next_state, disconnected, State}
  end.

handle_sync_event(todo_cmds, _From, StateName, #state{cmds=Cmds}=State) ->
  {reply, Cmds, StateName, State};
handle_sync_event(get_state, _From, StateName, State) ->
  {reply, StateName, StateName, State};
handle_sync_event(stop, _From, _StateName, #state{host=Host}=State) ->
  error_logger:info_msg("machine [~p] stop.", [Host]),
  {stop, normal, ok, State}.

handle_info(Info, StateName, #state{cm=Cm, handler=Handler, datas=Datas, exec_mod=ExecMod}=State) ->
  try ExecMod:handle_info(Info, Cm, Handler) of
    {data, Data} ->
      {next_state, StateName, State#state{datas=[Data | Datas]}};
    {exit_status, ExitStatus} ->
      {next_state, StateName, State#state{cmd_exit_status=ExitStatus}};
    eof ->
      % 如果被interrupt，有可能接受不到eof消息，所以不再这里反转和拼接消息，而是放在closed中。 
      {next_state, StateName, State};
    closed ->
      {NextState, NewState} = finish_cmd(StateName, State),
      {next_state, NextState, NewState}
  catch 
    error:function_clause -> 
      error_logger:warning_msg("~p recv msg: ~p, StateName=~p, State=~p~n", [?MODULE, Info, StateName, State]),
      {next_state, StateName, State}
  end.

terminate(_Reason, _StateName, #state{host=Host, exec_mod=ExecMod, cmds=Cmds, cm=Cm, responder_mod=RespMod}=_State) ->
  error_logger:info_msg("machine [~p] terminated, cmds=~p~n", [Host, Cmds]),
  recover:save(?SERVER(Host), Cmds),
  ExecMod:terminate(Cm),
  (RespMod:machine_on_caller(client))(Host, disconnect),
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
create_connection_manager(Host, GetHostInfoFun) ->
  case GetHostInfoFun(Host) of
    [] ->
      {error, no_host_info};
    Params ->
      ExecMod = get_exec_mod(Params),
      case ExecMod:conn_manager(Host, Params) of
        {ok, Cm} ->
          {ok, Cm, ExecMod, Params};
        {error, Why} ->
          {error, Why}
      end
  end.

finish_cmd(CurrentState, #state{host=Host, current_cmd=Cmd, cmd_exit_status=ExitStatus, datas=Datas, responder_mod=RespMod}=State) ->
  Msg = lists:append(lists:reverse(Datas)),
  IsOk = ExitStatus == 0,
  error_logger:info_msg("machine [~p] cmd callback, cmd=~p, result={~p, ~ts}~n", [Host, Cmd, IsOk, Msg]),
  (RespMod:cb_caller(client))(Host, Cmd, {IsOk, string:sub_string(Msg,1,51200)}), %% max body size: 51200
  NextState = case CurrentState of
    paused ->
      error_logger:info_msg("machine [~p] cmd callback, paused -> paused.~n", [Host]),
      paused;
    run ->
      case IsOk of
        true ->
          error_logger:info_msg("machine [~p] cmd callback true, run -> normal.~n", [Host]),
          do_cmd(Host),
          normal;
        false ->
          error_logger:info_msg("machine [~p] cmd callback false, run -> paused, why=~ts~n", [Host, Msg]),
          paused
      end
  end,
  {NextState, State#state{handler=undefined, current_cmd=undefined, datas=[], cmd_exit_status=undefined}}.

get_exec_mod(Params) ->
  case proplists:get_value(adapter, Params) of
    undefined -> ssh_executor;
    Exec -> list_to_atom(Exec ++ "_executor")
  end.
