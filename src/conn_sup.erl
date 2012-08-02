-module(conn_sup).
-behaviour(supervisor).
-export([start_link/1, start_child_if_not_exist/2, init/1]).

start_link(Responder_mod) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Responder_mod]).

start_child_if_not_exist(Host, GetHostInfoFun) ->
  case client:check_host(Host) of
    no_host -> 
      supervisor:start_child(?MODULE, [Host, GetHostInfoFun]);
    ok -> ok
  end.

init([Responder_mod]) ->
  ProcessSpec = {
    client,
    {client,start_link,[Responder_mod]},
    transient,
    2000,
    worker,
    [client]
  },
  http_channel_sup:start_child(client),
  {ok,{{simple_one_for_one,10,100}, [ProcessSpec]}}.

