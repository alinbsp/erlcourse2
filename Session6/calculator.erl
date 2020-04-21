-module(calculator).
-include_lib("eunit/include/eunit.hrl").
-export([start/0, add/2, extract/2, multiply/2, equal/1, reset/1, exit/1]).


start() ->
	spawn(fun init/0).

init() -> loop([]).

add(Pid, V) ->
	Pid ! { add, V}.

extract(Pid, V) ->
	Pid ! {extract, V}.

multiply(Pid, V) ->
	Pid ! {multiply, V}.

exit(Pid) ->
	Pid ! {quit}.

reset(Pid) ->
	Pid ! {reset}.

equal(Pid) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, equal},
	receive
		{Ref, Value} ->
			io:format("equal: ~p~n", [Value]);
		{'DOWN', Ref, process, Pid, Reason} ->
		      erlang:error(Reason)
	end.

loop(State) ->
	receive
		{add, V} ->
			loop([{add, V}|State]);
		{extract, V} ->
			loop([{extract, V}|State]);
		{multiply, V} ->
			loop([{multiply, V}|State]);
		{From, Ref, equal} ->
			From ! {Ref, calc(State)},
			loop([]);
		{reset} ->
			loop([]);
		{quit} ->
			io:format("calculator exit~n");
		Unknown ->
			io:format("unknown message: ~p~n", [Unknown])
	end.

calc(State) -> calc(State, 0).

calc([], Acc) -> Acc;
calc([{add, V}|Tail], Acc) -> calc(Tail, Acc + V);
calc([{extract, V}|Tail], Acc) -> calc(Tail, Acc - V).
 
calc_test() ->
	3 = calc([{add, 1}, {add, 2}, {add, 0}]),
	-2 = calc([{extract, 2}]),
	0 = calc([{multiply, 4}]),
	10 = calc([{add, 4}, {add, 2}, {multiply, 3}]),
	5 = calc([{add, 4}, {multiply, 2}, {multiply, 1}, {extract, 3}]).

