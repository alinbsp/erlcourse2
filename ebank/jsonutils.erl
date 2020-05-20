% -export([json/1]).
% -record(json, {list=[], raw=[]}).
% -record(jsonkv, {value=[], raw=[]}).

% json_value_quoted(Value, [$" | T]) ->
%   #jsonkv{value=Value, raw=T};

% json_value_quoted(Value, [Next | T]) ->
%   json_value_quoted(Value ++ [Next], T).

% json_value(Value, RawJSON) ->
%   [Next | T] = RawJSON, 
%   case Next of
%     $: -> throw('Unexpected');
%     ${ -> J = json(RawJSON),
%             #jsonkv{value=J#json.list, raw=J#json.raw};
%     $, -> #jsonkv{value=string:strip(Value), raw=RawJSON};
%     $} -> #jsonkv{value=string:strip(Value), raw=RawJSON};
%     $" -> json_value_quoted(Value, T);
%     _  -> json_value(Value ++ [Next], T)
%   end.

% json(JSON, Key) ->
%   [Next | T] = JSON#json.raw,
%   case {Next, T} of
%     {$", _} -> json(JSON#json{raw=T}, Key);        
%     {${, _} -> json(#json{raw=T}, []);
%     {$,, _} -> json(JSON#json{raw=T}, []);        
%     {$:, _} -> KV = json_value([], T),  
%             List = lists:merge(JSON#json.list, [{string:strip(Key), KV#jsonkv.value}]),
%             json(#json{list=List, raw=KV#jsonkv.raw}, []);  
%     {$}, []} -> JSON#json.list;                   
%     {$}, _} -> JSON#json{raw=T};                   
%     {_, _} -> json(JSON#json{raw=T}, Key ++ [Next])  
%   end.

% json(RawJSON) ->
%   json(#json{raw=RawJSON}, []).


% %%%%%%%%%%%
% %%%%%%%%%%%

% -record(foobar, {name, value}).
% -record(foo, {other, fields, 'and', stuff}).

% record_to_proplist(#foobar{} = Rec) ->
%   lists:zip(record_info(fields, foobar), tl(tuple_to_list(Rec)));

% record_to_proplist(#foo{} = Rec) ->
%   lists:zip(record_info(fields, foo), tl(tuple_to_list(Rec))).



% -define(R2P(Record), record_to_proplist(#Record{} = Rec) ->
%            lists:zip(record_info(fields, Record), tl(tuple_to_list(Rec)))).

% ?R2P(foobar).
% ?R2P(foo).