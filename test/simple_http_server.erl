-module(simple_http_server).
-export([start/0, start/1, stop/0]).

start() ->
    start(3000).

start(Port) ->
    case whereis(?MODULE) of
        undefined -> 
            case gen_tcp:listen(Port, [{active, false}]) of
                {ok, Listen} ->
                    Pid = spawn(fun () -> 
                        spawn_link(fun() -> loop(Listen) end),
                        receive
                            {stop, From} -> 
                                ok = gen_tcp:close(Listen),
                                From ! ok
                        end
                    end),
                    register(?MODULE, Pid),
                    ok;
                Others ->
                    Others
            end;
        _Pid -> 
            {error, {already_started, ?MODULE}}
    end.

stop() ->
    case whereis(simple_http_server) of
        undefined -> ok;
        _Pid ->
            simple_http_server ! {stop, self()},
            receive
                ok -> ok
            end
    end.

get_mapping() ->
    dict:from_list([
        {"/api/commands?room_name=CNZ&reload=true", "localhost:echo hello:1"}
    ]).

loop(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Handler = spawn(fun () -> handle(Socket) end),
            gen_tcp:controlling_process(Socket, Handler),
            loop(Listen);
        {error, closed} ->
            error_logger:info_msg("listen closed.~n")
    end.

handle(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, B} ->
            Path = get_path(B),
            error_logger:info_msg("path: ~p~n", [Path]),
            R = case dict:find(Path, get_mapping()) of
                {ok, V} -> V;
                error -> ""
            end,
            error_logger:info_msg("response: ~p~n", [R]),
            gen_tcp:send(Socket, response(R)),
            gen_tcp:close(Socket);
        {error, closed} ->
            io:format("socket closed.~n");
        Other ->
            io:format("socket Other: ~p~n", [Other])
    end.

get_path(Req) ->
    error_logger:info_msg("request: ~p~n", [Req]),
    {match,[_, _, {Start, Length}]} = re:run(Req, "^(GET|POST) (.*) HTTP/1.1", []),
    string:substr(Req, Start + 1, Length).

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [size(B), B])).
