-module(eclock).

-export([run_interval/3, spawn_interval/2, loop/2, stop/1 ,test/0, test2/0, test3/0]).

run_interval(_Task, _Interval, 0) ->
	ok;
run_interval(Task, Interval, Times) ->
	timer:sleep(Interval*1000),
	Task(),
	run_interval(Task, Interval, Times - 1).

spawn_interval(Task, Interval) ->
	spawn(?MODULE, loop, [Task, Interval]).

loop(Task, Interval) ->
	receive
		stop ->
			ok
	after Interval*1000 ->
		try
			Task()
		catch
			Type:Error ->
				error_logger:error_report([{Type, Error}])
		end,
		loop(Task, Interval)
	end.

stop(Clock) ->
	Clock ! stop.

test() ->
	io:format("Testing eclock ~n").

test2() ->
	io:format("second process~n").

test3() ->
	Res = 1/(random:uniform(2) - 1),
	io:format("test3 working ~p~n", [Res]).
