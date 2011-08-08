-module(eclock_raw).

-export([run_interval/3, spawn_interval/2, loop/2, stop/1 ,test/0, test2/0, test3/0]).

-spec run_interval(fun(()-> any()), non_neg_integer(), non_neg_integer()) -> ok.
run_interval(_Task, _Interval, 0) ->
	ok;
run_interval(Task, Interval, Times) ->
	timer:sleep(Interval*1000),
	Task(),
	run_interval(Task, Interval, Times - 1).

-spec spawn_interval(fun(()-> any()), non_neg_integer()) -> pid().
spawn_interval(Task, Interval) ->
	spawn(?MODULE, loop, [Task, Interval]).

-spec loop(fun(()-> any()), non_neg_integer()) -> ok.
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

-spec stop(pid()) -> stop.
stop(Clock) ->
	Clock ! stop.

-spec test() -> ok.
test() ->
	io:format("Testing eclock ~n").

-spec test2() -> ok.
test2() ->
	io:format("second process~n").

-spec test3() -> ok.
test3() ->
	Res = 1/(random:uniform(2) - 1),
	io:format("test3 working ~p~n", [Res]).
