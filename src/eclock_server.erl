%%% -------------------------------------------------------------------
%%% Author  : marcosa
%%% Description :
%%%
%%% Created : 05/08/2011
%%% -------------------------------------------------------------------
-module(eclock_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {id, task, timeout, executed_times=0}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Task,Interval) ->
	gen_server:start_link(?MODULE,[Task, Interval], []).

stop(Pid) when is_pid(Pid) -> 
	gen_server:cast(Pid, stop);
stop(Id) ->	
	case eclock_db:lookup(Id) of 
		not_found -> throw({not_found, Id});
		{Pid, _, _} -> stop(Pid)
	end.
					 
%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Task, Timeout]) ->
	Id = eclock_db:insert({self(), Task,Timeout}),
    {ok, #state{id=Id,task=Task,timeout=Timeout*1000},Timeout*1000}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
	io:format("The task was executed ~p times.~n", [State#state.executed_times]),
	eclock_db:delete(State#state.id),
    {stop, normal, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) ->
	Task = State#state.task,
	ExecutedTimes = State#state.executed_times,
	try
		Task()
	catch
		Type:Error ->
			error_logger:error_report([{Type, Error}])
	end,
	{noreply, State#state{executed_times= ExecutedTimes+1}, State#state.timeout};
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
