

-module(ebank_web).
-compile(tuple_calls).
-include_lib("include/account.hrl").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mnesia:start(),        
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mnesia:stop(),
    mochiweb_http:stop(?MODULE).

%Erlang data structures to JSON

to_json(Int) when is_integer(Int) ->
    integer_to_list(Int);

to_json(Float) when is_float(Float) ->
    float_to_list(Float);

to_json(Atom) when is_atom(Atom) -> 
    to_json(atom_to_binary(Atom, latin1));

to_json(Bin) when is_binary(Bin) ->
    to_json_key(Bin);

to_json(Proplist = [{_,_}|_]) ->
    to_json_prop(Proplist);

to_json(List = [_|_]) ->
    to_json_list(List);

to_json(Tuple) when is_tuple(Tuple) ->
    to_json(tuple_to_list(Tuple));

to_json(Map) when is_map(Map) ->
    to_json_prop(maps:to_list(Map)). 

to_json_key(Int) when is_integer(Int) ->
    [$\", integer_to_list(Int), $\"];

to_json_key(Float) when is_float(Float) ->
    [$\", float_to_list(Float), $\"];

to_json_key(Atom) when is_atom(Atom) -> 
    to_json(atom_to_binary(Atom, latin1));

to_json_key(Bin) when is_binary(Bin) ->
    [$\", binary_to_list(Bin), $\"].

%to_json_key(Proplist = [{_,_}|_]) ->
%    [$\", Proplist, $\"];

%to_json_key(List = [_|_]) ->
%    [$\", List, $\"];

%to_json_key(Tuple) when is_tuple(Tuple) ->
%    to_json_key(tuple_to_list(Tuple)).


to_json_prop(L) -> to_json_prop(L, []).
to_json_prop([], Acc) -> [${, Acc, $}];
to_json_prop([{K,V}|T], Acc) when length(T) =/= 0 -> 
    to_json_prop(T, Acc ++ [to_json_key(K), $:, to_json(V), $,]);
to_json_prop([{K,V}|T], Acc) when length(T) =:= 0 -> 
    to_json_prop(T, Acc ++ [to_json_key(K), $:, to_json(V)]).

to_json_list(L) -> to_json_list(L, []).
to_json_list([], Acc) -> [$[, Acc, $]];
to_json_list([H|T], Acc) when length(T) =/= 0 ->
    to_json_list(T, Acc ++ [to_json(H), $,]);
to_json_list([H|T], Acc) when length(T) =:= 0 ->
    to_json_list(T, Acc ++ [to_json(H)]).

rec2json(#account{id=Id, details=#accountDetails{name=Name, balance=Balance, pin=Pin}}) ->
    to_json([{id,Id},{name, Name}, {balance, Balance}, {pin, Pin}]).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                  "hello_world" ->
                        Req:respond({200, [{"Content-Type", "text/plain"}], "Hello world!\n"});
                  "test_toJson" ->
                        Req:respond({200, [{"Content-Type", "text/plain"}], to_json([{<<"key">>, value}, {key2, 2}, {22, [123,atom]}])});  
		          "getBalance" ->
	                    Accounts = mochiglobal:get(accounts),
		                Req:respond({200, [{"Content-Type", "text/plain"}], "{\"Balance\": 100}\n"});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
		            "create" ->
                        QueryData = Req:parse_qs(),
                        %[{id, Id}, {name, Name}, {pin, Pin}, {balance, Balance}] = QueryData,
                        QueryKeys = proplists:get_keys(QueryData),
                        [Id, Name, Balance, Pin] = lists:map(fun(X) -> proplists:get_value(X, QueryData) end, QueryKeys),
                        Adet = #accountDetails{name=Name, balance=Balance, pin=Pin},
                        Account = #account{id=Id, details=Adet},
                        ResponseStatus = save_account(Account),
                        Req:respond({200, [{"Content-Type", "text/plain"}], "{\"Status\": Ok}\n"});
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
                      error_logger:error_report(Report),
                      Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

save_account(Account) ->
   {atomic, Res} = mnesia:transaction(fun() -> mnesia:write(Account) end),
   Res.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
