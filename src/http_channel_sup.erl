-module(http_channel_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/1, init/1]).

start_link() ->
  inets:start(permanent),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ProfileName) ->
  supervisor:start_child(?MODULE, [ProfileName]).

init([]) ->
  ProcessSpec = {
    webutil,
    {webutil, start_link, []},
    transient,
    2000,
    worker,
    [webutil]
  },
  {ok,{{simple_one_for_one,10,60}, [ProcessSpec]}}.
