-module(recover).
-export([start/0, stop/0, save/2, recover/2]).
-define(SERVER(), recover_ets).

start() ->
  ets:new(?SERVER(), [public, named_table]),
  ok.

stop() ->
  ets:delete(?SERVER()),
  ok.

save(Id, Value) ->
  ets:insert(?SERVER(), {Id, Value}).

recover(Id, Default_v) ->
  case ets:lookup(?SERVER(), Id) of
    [] -> Default_v;
    [{Id, V}] ->
      ets:delete(?SERVER(), Id),
      V
  end.
