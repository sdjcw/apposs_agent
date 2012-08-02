-module(lib_misc).
-export([invoke_do_my_best/3, invoke_do_my_best/4, parse_string/1, join_params/1, string_to_utf8/1, list_to_hex/1]).
-compile(export_all).

%% 尽力尝试调用某方法，用于类似与http请求这类可能临时失败的情况
%% F 需要调用的方法， Count 尝试次数， Delay 调用失败的间隔
invoke_do_my_best(F, Count, Delay) ->
  invoke_do_my_best(F, 1, Count, Delay).

invoke_do_my_best(F, Index, Max_count, Delay) ->
  case F() of
  {error, Reason} -> 
    error_logger:warning_msg("Invoke function fail, Reason: ~p~nTry count: ~p.~n", [Reason, Index]),
    if 
      Index =:= Max_count ->
        {error, Reason};
      true ->
        timer:sleep(Delay),
        invoke_do_my_best(F, Index + 1, Max_count, Delay)
    end;
  _Other ->
    _Other
  end.

%% 将字符串"a=b,c=d,x=y"解析为：[{a,"b"},{c,"d"},{x,"y"}]
parse_string(Str) ->
  ArgPairs = string:tokens(Str,","),
  Result = lists:foldl(
    fun(E, AccIn) ->
      case string:tokens(E,"=") of
        [_K] -> AccIn;
        [_K,""|_] -> AccIn;
        [_K,"null"|_] -> AccIn;
        [K,V|_]  -> lists:append(AccIn,[{list_to_atom(K),V}])
      end
    end,
    [],
    ArgPairs
  ),
  error_logger:info_msg("Parse string: ~p~nTo: ~p~n",[Str, Result]),
  Result.


%% 将列表[{a,"b"},{c,"d"},{x,"y"}]拼接为a=b&c=d&x=y格式，
%% 其中value将会被转码为UTF8格式，如“中文”会被转换为"%E4%B8%AD"，用于url请求 
join_params(Ps) ->
  join_params(Ps, "").

join_params([],Acc) ->
    Acc;
join_params([{Key,Value} | R], "") ->
  join_params(R, atom_to_list(Key) ++ "=" ++ any_to_string(Value));
join_params([{Key,Value} | R], Acc) ->
  join_params(R, Acc ++ "&" ++ atom_to_list(Key) ++ "=" ++ any_to_string(Value)).

any_to_string(Value) ->
  if
    is_integer(Value) -> integer_to_list(Value);
    is_atom(Value) ->
      atom_to_list(Value);
    true -> 
      string_to_utf8(Value)
  end.

string_to_utf8(L) ->
  lists:foldl(fun(X, Result) -> Result ++ "%" ++ X  end, "", list_to_hex(L)). 

list_to_hex(L) ->
  lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(X) when X < 256 ->
  [hex(X div 16), hex(X rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $A+(N-10). 
