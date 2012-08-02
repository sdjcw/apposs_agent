-module(webutil).
-behaviour(gen_server).
-define(SERVER(ProfileName), list_to_atom(atom_to_list(ProfileName) ++ "@" ++ atom_to_list(?MODULE))).
-define(DEFAULT_HTTP_OPTIONS, [{connect_timeout, 500}]).
-define(DEFAULT_OPTIONS, [{sync,true}]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,http_get/2,http_get/3,http_get/4,http_post/3,http_post/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ProfileName) ->
  gen_server:start_link({local, ?SERVER(ProfileName)}, ?MODULE, [ProfileName], []).

http_get(ProfileName, Url) ->
  http_get(ProfileName, Url,[],fun(Body) -> Body end).

http_get(ProfileName, Url,Params) ->
  http_get(ProfileName, Url,Params,fun(Body) -> Body end).

http_get(ProfileName, Url,Params,CbFunc) ->
  gen_server:call(?SERVER(ProfileName), {http_get, Url, Params, CbFunc}, 10000).

http_post(ProfileName, Url,Params) ->
  http_post(ProfileName, Url,Params,fun(Body) -> Body end).

http_post(ProfileName, Url,Params,CbFunc) ->
  gen_server:call(?SERVER(ProfileName), {http_post, Url, Params, CbFunc}, 10000).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ProfileName]) ->
  inets:start(httpc, [{profile, ProfileName}]),
  {ok, [ProfileName]}.

handle_call({http_get, Url, Params, CbFunc}, _From, [ProfileName]) ->
  Reply = http_response(Url, Params, CbFunc, httpc:request(
            get,
            {Url ++ "?" ++ lib_misc:join_params(Params),[]},
            ?DEFAULT_HTTP_OPTIONS,
            ?DEFAULT_OPTIONS,
            ProfileName
        )),
  {reply, Reply, [ProfileName]};
handle_call({http_post, Url, Params, CbFunc}, _From, [ProfileName]) ->
  ContentType = "application/x-www-form-urlencoded",
  Reply = http_response(Url, Params, CbFunc, httpc:request(
            post,
            { Url, [], ContentType, lib_misc:join_params(Params) },
            ?DEFAULT_HTTP_OPTIONS,
            ?DEFAULT_OPTIONS,
            ProfileName
        )),
  {reply, Reply, [ProfileName]}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, [ProfileName]) ->
  inets:stop(httpc, ProfileName),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


http_response(Url, Params, F,{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}) ->
  error_logger:info_msg("http req: ~p(~p)~nresp: ~p~n", [Url, Params, Body]),
  {ok, F(Body)};
http_response(Url, Params, _F,{ok, {{_Version, Status, ReasonPhrase}, _Headers, Body}}) ->
  error_logger:error_msg("http fail: ~p(~p) ~p: ~p~nresp: ~p~n",[Url, Params, Status, ReasonPhrase, Body]),
  {error,Status};
http_response(Url, Params, _F,{error, Reason}) ->
  error_logger:error_msg("http error: ~p(~p) ~p~n",[Url, Params, Reason]),
  {error,Reason}.

