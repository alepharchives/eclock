%% Created: 11/08/2011
%% Description: TODO: Add description to eclock_db
-module(eclock_db).

-record(eclock,{id, pid, task, interval}).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, insert/1,lookup/1,delete/1]).

%%
%% API Functions
%%
start() ->
	ets:new(?MODULE, [ordered_set, named_table,{keypos,#eclock.id},public]).

insert({Pid, Task, Interval}) ->
	Id = case ets:last(?MODULE) of
			 '$end_of_table' -> 1;
			 NewId -> NewId + 1
		 end,
	true = ets:insert(?MODULE, #eclock{id=Id, pid=pid_to_list(Pid),task=Task, interval=Interval}),
	Id.

lookup(Id)->
	case ets:lookup(?MODULE, Id) of
		[] -> not_found;
		[#eclock{pid=Pid, task=Task, interval= Interval }]-> {list_to_pid(Pid), Task, Interval}
	end.

delete(Id)->
	true = ets:delete(?MODULE, Id).
%%
%% Local Functions
%%

