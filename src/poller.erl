%% 负责周期性的获取command，并发送给 agent_server
-module(poller).
-behaviour(gen_server).
-export([start_link/3,init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).
-define(POLLER_PID(RoomName), list_to_atom(string:concat("poller:", RoomName))).

-ifdef(TEST).
-compile(export_all).
-endif.

start_link(Delay_time, BaseUrl,RoomName) ->
  gen_server:start_link({local, ?POLLER_PID(RoomName)}, ?MODULE, [BaseUrl, RoomName, Delay_time], []).

init([BaseUrl, RoomName, Delay_time]) ->
  error_logger:info_msg("start poller: ~p - ~p.~n",[BaseUrl,RoomName]),
  gen_server:cast(?POLLER_PID(RoomName), {reload, Delay_time}),
  {ok,[BaseUrl, RoomName]}.

handle_call(_Req, _From, State) ->
  {stop, unknown_req, State}.

handle_cast({reload, Delay_time}, [BaseUrl, RoomName]) ->
  error_logger:info_msg("poller reload: ~p - ~p.~n",[BaseUrl,RoomName]),
  %% 检查系统是否有剩余的命令，如果没有，则可能是初次启动，此时应该重新装载所有download和init命令
  Params = [{room_name,RoomName},{reload,true}],
  run(BaseUrl,Params),
  timer:sleep(Delay_time),
  gen_server:cast(?POLLER_PID(RoomName), {poll, Delay_time}),
  {noreply, [BaseUrl,RoomName]};
handle_cast({poll, Delay_time}, [BaseUrl,RoomName]) ->
  Params = [{room_name,RoomName}],
  run(BaseUrl,Params),
  timer:sleep(Delay_time),
  gen_server:cast(?POLLER_PID(RoomName), {poll, Delay_time}),
  {noreply, [BaseUrl,RoomName]}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  error_logger:info_msg("reason: ~p~n", [Reason]),
  error_logger:info_msg("state: ~p~n",  [State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

run(BaseUrl, Params)->
  case get_cmds(BaseUrl, Params) of
    [] -> ok;
    Cmds ->
      create_clients(BaseUrl, Cmds),
      dispatch_cmd(Cmds)
  end.

get_cmds(BaseUrl, Params) ->
  Url = BaseUrl ++ "/api/commands",
  case webutil:http_get(poller, Url, Params, 
      fun(E) ->
        analyse_cmds(E)
      end
  ) of
    {ok, Cmds} -> Cmds;
    {error, Why} ->
      error_logger:error_msg("poller get_cmds failed, reqUrl: ~p, Why: ~p~n", [Url, Why]),
      []
  end.

analyse_cmds("") -> "";
analyse_cmds("ok") -> "";
analyse_cmds(Cmds) ->
  Tokens = lists:filter(fun(E)-> string:len(E) > 0 end, string:tokens(Cmds,"\n")),
  lists:reverse(lists:foldl(
    fun(E,AccIn) ->
      error_logger:info_msg("analyse command line: ~p~n", [E]),
      case [Host | TempList] = string:tokens(E, ":") of
        [Host | TempList] ->
          Oid = lists:last(TempList),
          Command = string:join(lists:sublist(TempList,1,length(TempList)-1),":"),
          [{Host, Command, Oid} | AccIn];
        _Other ->
          error_logger:error_msg("bad cmd: ~p~n", [E])
      end
    end,
    [],
    Tokens
  )).

create_clients(BaseUrl, Cmds) ->
  F = fun(Host) ->
    {ok, Result} = webutil:http_get(
      poller,
      BaseUrl ++ "/api/load_hosts",
      [{hosts, Host}],
      fun
        ("") -> [];
        ("ok") -> [];
        (Hosts) ->
          [HostLine | _] = lists:filter(fun(E)-> string:len(E) > 0 end, string:tokens(Hosts,"\n")),
          error_logger:error_msg("host info not unique: ~p~n", [Hosts]),
          lib_misc:parse_string(HostLine)
      end
    ),
    Result
  end,
  lists:foreach(
    fun({Host, _, _}) ->
      conn_sup:start_child_if_not_exist(Host, F)
    end,
    Cmds
  ). 

%% 合理输入：[{"localhost", "stop", "1"}, {"localhost", "redeploy", "2"}, {"test", "start", "3"}]
dispatch_cmd(Cmds) ->
  lists:foreach(
    fun({Host, Command, Oid}) ->
        dispatcher:dispatch(Host, Command, Oid)
    end, Cmds).
