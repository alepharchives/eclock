-module(eclock).

-export([run_interval/3, test/0]).

run_interval(_Task, _Interval, 0) ->
	ok;
run_interval(Task, Interval, Times) ->
	timer:sleep(Interval*1000),
	Task(),
	run_interval(Task, Interval, Times - 1).

test() ->
	io:format("Testing eclock ~n").
