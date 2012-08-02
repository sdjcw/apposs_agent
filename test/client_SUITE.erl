%% common_test suite for client

-module(client_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() -> [{timetrap, {seconds, 10}}].

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() -> [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%
%%      NB: By default, we export all 1-arity user defined functions
%%--------------------------------------------------------------------
all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
                               fun ({module_info,_}) -> false;
                                   ({all,_}) -> false;
                                   ({init_per_suite,1}) -> false;
                                   ({end_per_suite,1}) -> false;
                                   ({_,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Host = "127.0.0.1",
    GetHostInfoFun = fun(_Host) ->
      [{user,"lifu"},{password,"hello1234"}]
    end,
    [{host, Host}, 
     {get_host_info_fun, GetHostInfoFun}, 
     {responder_mod, responder_mock} 
     | Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(_group, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    recover:start(),
    crypto:start(),
    ssh:start(),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
  Host = ?config(host, Config),
  case client:check_host(Host) of
    ok ->
      client:stop(Host);
    _ -> ok
  end,
  recover:stop(),
  Config.

test_add_and_do_cmd(Config) ->
  Host = ?config(host, Config),
  client:start(?config(responder_mod, Config), Host, ?config(get_host_info_fun, Config)),
  client:add_cmd(Host, {"echo hello", 1}),
  client:add_cmd(Host, {"echo world", 2}),
  timer:sleep(1500),
  [] = client:todo_cmds(Host),
  normal = client:get_state(Host),
  client:stop(Host),
  no_host = client:check_host(Host).

test_repeat_add_cmd(Config) ->
  Host = ?config(host, Config),
  client:start(?config(responder_mod, Config), Host, ?config(get_host_info_fun, Config)),
  % read会一直阻塞不结束，并且一直作为current_cmd而存在，后续的指令一直保存在待运行列表中
  client:add_cmd(Host, {"read", 1}),
  client:add_cmd(Host, {"echo hello", 2}),
  client:add_cmd(Host, {"echo world", 3}),
  timer:sleep(200),
  % 添加的指令域current_cmd一致，所以忽略
  client:add_cmd(Host, {"read", 1}),
  timer:sleep(200),
  [{"echo hello", 2}, {"echo world", 3}] = client:todo_cmds(Host),
  % 重复添加的指令，因为指令列表中已经存在，所以忽略 
  client:add_cmd(Host, {"echo hello", 2}),
  timer:sleep(200),
  [{"echo hello", 2}, {"echo world", 3}] = client:todo_cmds(Host),

  % 指令虽然相同，但oid不同，仍然认为是不同指令
  client:add_cmd(Host, {"echo hello", 4}),
  timer:sleep(200),
  [{"echo hello", 2}, {"echo world", 3}, {"echo hello", 4}] = client:todo_cmds(Host),
  client:stop(Host).

test_pause_client(Config) ->
  Host = ?config(host, Config),
  client:start(?config(responder_mod, Config), Host, ?config(get_host_info_fun, Config)),
  % normal状态下pause
  client:pause(Host),
  paused = client:get_state(Host),
  
  client:reset(Host),
  client:add_cmd(Host, {"echo sleep && sleep 2", 1}),
  client:add_cmd(Host, {"echo world", 2}),
  timer:sleep(1500),
  run = client:get_state(Host),
  % run状态下pause
  client:pause(Host),
  paused = client:get_state(Host),
  
  % paused状态下pause
  client:pause(Host),
  % 发出pause指令，当前指令执行完后，后续指令不会自动执行
  timer:sleep(1500),
  [{"echo world", 2}] = client:todo_cmds(Host),
  client:stop(Host).

test_interrupt(Config) ->
  Host = ?config(host, Config),
  client:start(?config(responder_mod, Config), Host, ?config(get_host_info_fun, Config)),
  % interrupt在normal state，后续指令不会自动执行
  client:interrupt(Host),
  timer:sleep(500),
  paused = client:get_state(Host),
 
  client:reset(Host),
  timer:sleep(500),
  % interrupt在run state，会强行终止当前指令，并且后续指令不会自动执行
  client:interrupt(Host),
  timer:sleep(500),
  paused = client:get_state(Host),
 
  % interrupt在pause state
  client:interrupt(Host),

  client:reset(Host),
  client:add_cmd(Host, {"read", 1}),
  timer:sleep(500),
  % 因为有read指令，所以即使后续处于paused状态，仍然是有一条在执行指令的
  % 所以interrupt在这个时候应该同样能起到中断的作用
  client:pause(Host),
  client:interrupt(Host),
  % 因为interrupt以后，就没有“正在运行”的指令，所以reset后client处于normal状态
  client:reset(Host),
  timer:sleep(500),
  normal = client:get_state(Host),
  client:stop(Host).

test_reset_client(Config) ->
  Host = ?config(host, Config),
  client:start(?config(responder_mod, Config), Host, ?config(get_host_info_fun, Config)),
  % reset在normal state 
  client:reset(Host),
  
  client:add_cmd(Host, {"echo sleep && sleep 1 && echo sleepEnd", 1}),
  client:add_cmd(Host, {"echo hello", 1}),
  timer:sleep(500),
  % reset在run state
  client:reset(Host),
  
  client:pause(Host),
  timer:sleep(1500),
  [{"echo hello", 1}] = client:todo_cmds(Host),
  % reset在pause state
  client:reset(Host),
  timer:sleep(1500),
  [] = client:todo_cmds(Host),

  % 耗时长的指令未执行完就被reset，应该直接处于run状态，current_cmd仍是未执行完的指令
  client:add_cmd(Host, {"read", 1}),
  client:add_cmd(Host, {"echo hello", 2}),
  timer:sleep(200),
  client:pause(Host),
  client:reset(Host),
  % reset后，current_cmd仍然为“read”，无限阻塞
  timer:sleep(1500),
  [{"echo hello", 2}] = client:todo_cmds(Host),
  client:stop(Host).

test_clean_cmds(Config) ->
  Host = ?config(host, Config),
  client:start(?config(responder_mod, Config), Host, ?config(get_host_info_fun, Config)),
  % clean_cmds在normal state
  client:clean_cmds(Host),
  [] = client:todo_cmds(Host),
  
  client:pause(Host),
  client:add_cmd(Host, {"echo hello", 1}),
  client:add_cmd(Host, {"echo world", 2}),
  [{"echo hello", 1}, {"echo world", 2}] = client:todo_cmds(Host),
  % clean_cmds在pause state
  client:clean_cmds(Host),
  timer:sleep(100),
  [] = client:todo_cmds(Host),

  client:reset(Host),
  client:add_cmd(Host, {"echo sleep && sleep 1 && echo sleepEnd", 1}),
  client:add_cmd(Host, {"echo hello", 2}),
  timer:sleep(100),
  [{"echo hello", 2}] = client:todo_cmds(Host),
  client:clean_cmds(Host),
  timer:sleep(100),
  [] = client:todo_cmds(Host),
  client:stop(Host).

test_command_error(Config) ->
  Host = ?config(host, Config),
  client:start(?config(responder_mod, Config), Host, ?config(get_host_info_fun, Config)),
  client:add_cmd(Host, {"error command", 1}),
  timer:sleep(1500),
  ok = client:check_host(Host),
  client:stop(Host).  

test_init_with_others_state(Config) ->
  Host = ?config(host, Config),
  client:start(?config(responder_mod, Config), Host, 
    fun(_H) -> 
      [{user,"lifu"},{password,"hello1234"},{state,"paused"}] 
    end),
  paused = client:get_state(Host),
  client:stop(Host),
 
  client:start(?config(responder_mod, Config), Host, 
    fun(_H) -> 
      [{user,"lifu"},{password,"hello1234"},{state,"disconnected"}] 
    end),
  normal = client:get_state(Host),
  client:stop(Host).

% 进程非正常退出时，未执行的命令会在下次启动时恢复
test_recover_cmds(Config) ->
  Host = ?config(host, Config),
  client:start(?config(responder_mod, Config), Host, ?config(get_host_info_fun, Config)),
  client:pause(Host),
  client:add_cmd(Host, {"echo 1", 1}),
  client:add_cmd(Host, {"echo 2", 2}),
  
  % 发送一条不存在的事件，使进程异常退出
  try gen_fsm:sync_send_event(list_to_atom(Host ++ "@" ++ "client"), xxxxx) of
    _ -> throw("should throw exception.")
  catch
    exit:_ -> ok
  end,
  client:start(?config(responder_mod, Config), Host, ?config(get_host_info_fun, Config)),
  [{"echo 1", 1}, {"echo 2", 2}] = client:todo_cmds(Host),
  client:stop(Host).

test_disconnected_state(Config) ->
  Host = ?config(host, Config),
  client:start(?config(responder_mod, Config), Host, fun(_) -> [] end),
  timer:sleep(100),
  disconnected = client:get_state(Host),

  client:add_cmd(Host, {"echo 1", 1}),
  timer:sleep(500),
  disconnected = client:get_state(Host),

  client:pause(Host),
  timer:sleep(500),
  disconnected = client:get_state(Host),
  
  client:interrupt(Host),
  timer:sleep(500),
  disconnected = client:get_state(Host),

  client:stop(Host).

% client在接收到消息是会出发handle_info方法，默认情况直接交给ExecMod进行处理。但有可能是其他额外消息，
% 要保证收到其他消息时client不会匹配错误而挂掉。
test_recv_others_msg(Config) ->
  Host = ?config(host, Config),
  {ok, Pid} = client:start(?config(responder_mod, Config), Host, ?config(get_host_info_fun, Config)),
  monitor(process, Pid),
  Pid ! {self(), "others msg"},
  receive
    _M -> throw("client is down, test fail.")
  after 1000 ->
    ok
  end.
