-module(responder).
-behaviour(gen_server).
-export([cb_caller/1, run_caller/1, machine_on_caller/1]).
-export([start_link/1,init/1,handle_call/3,handle_cast/2,handle_info/2]).
-export([code_change/3,terminate/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

start_link(BaseUrl) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [BaseUrl], []).

init([BaseUrl]) ->
  error_logger:info_msg("~p init, BasrUrl = ~p~n", [?MODULE, BaseUrl]),
  % responder使用一个http通道
  % http_channel_sup:start_child(responder),
  {ok, BaseUrl}.

run_caller(Profile) ->
  gen_server:call(?MODULE, {make_run_caller,Profile}).

cb_caller(Profile) ->
  gen_server:call(?MODULE, {make_cb_caller,Profile}).

% 目标机器动作的回调接口
machine_on_caller(Profile) ->
  gen_server:call(?MODULE, {make_machine_on_caller,Profile}).

handle_call({make_run_caller,Profile}, _From, BaseUrl) ->
  Reply = fun(Host, {_Cmd, OperationId}) -> 
    Url = BaseUrl++"/api/run",
    error_logger:info_msg("run:[~p,~p] ~p.~n",[Host,OperationId,Url]),
    F = fun() ->
      webutil:http_post(Profile, Url, [{host,Host},{oid,OperationId}])
    end,
    lib_misc:invoke_do_my_best(F, 10, 3000)
  end,
  {reply, Reply, BaseUrl};
handle_call({make_cb_caller,Profile}, _From, BaseUrl) ->
  Reply = fun(Host, {_Cmd, OperationId}, {IsOk, Body}) -> 
    Url = BaseUrl++"/api/callback",
    error_logger:info_msg("callback: ~p.~n",[Url]),
    F = fun() ->
      webutil:http_post(Profile, Url, [
        {isok,atom_to_list(IsOk)},
        {host,Host},
        {oid,OperationId},
        {body,Body}
      ])
    end,
    lib_misc:invoke_do_my_best(F, 10, 3000)
  end,
  {reply, Reply, BaseUrl};
handle_call({make_machine_on_caller,Profile}, _From, BaseUrl) ->
  Reply = fun(Host, Event) ->
    error_logger:info_msg("make_machine_on_caller: ~p , ~p~n",[Host, Event]),
    F = fun() ->
      webutil:http_post(Profile, BaseUrl ++ "/api/machine_on", [{host, Host}, {event, Event}])
    end,
    lib_misc:invoke_do_my_best(F, 10, 3000)
  end,
  {reply, Reply, BaseUrl}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
