-module(eclock_raw_sup).

-export([start/0, spawn_interval/2, loop/1]).

-spec start() -> true.
start() ->
	register(?MODULE,spawn(?MODULE, loop, [[]])).

-spec loop([]|list({pid(), fun(()-> any()), non_neg_integer()})) ->  no_return().
loop(Eclocks) ->
	process_flag(trap_exit, true),
	NewEclocks = 
		receive
			{spawn_interval, Task, Interval, From} ->
				link(Pid = eclock_raw:spawn_interval(Task, Interval)),
				From ! Pid,
				[{Pid, Task, Interval}|Eclocks];
			{'EXIT',_,normal} ->
				Eclocks;
			{'EXIT',FromPid,Reason} ->
				error_logger:error_report([{'EXIT',FromPid},{reason, Reason}]),
				case lists:keyfind(FromPid, 1, Eclocks) of
					false ->
						Eclocks;
					{FromPid, Task,Interval} ->
						link(Pid = eclock_raw:spawn_interval(Task, Interval)),
						io:format("Creating new process...~n"),
						[{Pid, Task, Interval}|Eclocks]
				end
		end,
	
	loop(NewEclocks).

-spec spawn_interval(fun(()-> any()), non_neg_integer()) -> pid().
spawn_interval(Task, Interval) ->
	?MODULE ! {spawn_interval, Task, Interval, self()},
	receive
		Pid ->
			Pid
	end.
