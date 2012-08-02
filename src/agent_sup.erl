%% 应用的主supervisor
-module(agent_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/3, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Fun, Args), {I, {I, Fun, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  start_link("http://localhost:3000", responder, 5000).

start_link(Center_url, Responder_mod, Poll_delay) ->
  crypto:start(),
  ssh:start(permanent),
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Center_url, Responder_mod, Poll_delay]).

stop() ->
  ChildSpecs = supervisor:which_children(?MODULE),
  lists:foreach(
    fun
      ({Id, _Child, _Type, _Modules}) -> 
        supervisor:terminate_child(?MODULE, Id),
        supervisor:delete_child(?MODULE, Id);
      (_E) -> ok
    end,
    ChildSpecs
  ),
  inets:stop().
  
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Center_url, Responder_mod, Poll_delay]) ->
  HttpChannelSup = ?CHILD(http_channel_sup, supervisor, start_link, []),
  Responder = ?CHILD(responder, worker, start_link, [Center_url]),
  ConnSup = ?CHILD(conn_sup, supervisor, start_link, [Responder_mod]),
  PollerSup = ?CHILD(poller_sup, supervisor, start_link, [Poll_delay]),
  {ok, { {one_for_one, 3, 30}, [HttpChannelSup, Responder,ConnSup,PollerSup]} }.
