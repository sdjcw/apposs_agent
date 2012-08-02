-module(agent_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  recover:start(),
  {ok, Center_url} = application:get_env(agent, center_url),
  {ok, Responder_mod} = application:get_env(agent, responder_mod),
  {ok, Poll_delay} = application:get_env(agent, poll_delay),
  {ok, Pid} = agent_sup:start_link(Center_url, Responder_mod, Poll_delay),
  {ok, Rooms} = application:get_env(agent, rooms),
  lists:foreach(fun(Room) ->
    error_logger:info_msg("add poller: ~p - ~p~n", [Center_url, Room]),
    supervisor:start_child(poller_sup,[Center_url, Room])
  end, Rooms),
  {ok, Pid}.

stop(_State) ->
  agent_sup:stop(),
  recover:stop(),
  ok.
