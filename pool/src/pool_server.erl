-module(pool_server).

-behaviour(gen_server).

%% API
-export([start/4, stop/1, start_link/4, run/2, async/2, sync/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {parent, supworkers, mfa, refs, queue, n}).

-define(SUPWORKER(MFA), #{
    id => supervisor_workers,
    start => {supervisor_workers, start_link, [MFA]},
    restart => transient, % permanent | transient | temporary
    shutdown => 2000,
    type => supervisor, % worker | supervisor
    modules => [supervisor_workers]
}).

start(Name, Parent, MFA, Limit) ->
    gen_server:start({local, Name}, ?MODULE, [Parent, MFA, Limit]).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name, Parent, MFA, Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, [Parent, MFA, Limit], []).

init([Parent, MFA, Limit]) ->
    self() ! {add_supervisor_worker},
    {ok, #state{parent=Parent, mfa=MFA, refs=gb_sets:empty(), queue=queue:new(), n=Limit}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({add_task, Args}, _From, State=#state{refs=Set, n=N, queue=_Queue, supworkers=Sup}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, ok, State#state{refs=gb_sets:add(Ref, Set), n=N-1}};

handle_call({add_task, _}, _From, State=#state{n=N}) when N == 0 ->
    {reply, noalloc, State};

handle_call({sync, Args}, _From, State=#state{refs=Set, n=N, queue=_Queue, supworkers=Sup}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, ok, State#state{refs=gb_sets:add(Ref, Set), n=N-1}};

handle_call({sync, Args}, From, State=#state{queue=Queue, n=N}) when N == 0 ->
    {noreply, State#state{queue=queue:in({From, Args}, Queue)}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({async, Args}, State=#state{refs=Set, n=N, queue=_Queue, supworkers=Sup}) when N > 0->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {noreply, State#state{refs=gb_sets:add(Ref, Set), n=N-1}};

handle_cast({async, Args}, State=#state{n=N, queue=Queue}) when N == 0->
    {noreply, State#state{queue=queue:in({noreply, Args}, Queue)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({add_supervisor_worker}, State=#state{parent=Parent, mfa=MFA})->
    {ok, PidSupWorkers} = supervisor:start_child(Parent, ?SUPWORKER(MFA)),
    {noreply, State#state{supworkers=PidSupWorkers}};

handle_info({'DOWN', Ref, process, _, normal}, State=#state{refs=Set, n=N, queue=Queue, supworkers=Sup})->
    case gb_sets:is_member(Ref, Set) of 
        false -> 
            {noreply, State};
        true->
            NewSet = gb_sets:del_element(Ref, Set),
            case queue:is_empty(Queue) of
                true ->
                    {noreply, State#state{n=N+1}};
                false->
                    {{value, {From, Args}}, Q2} = queue:out(Queue),
                    if 
                        From =/= noreply -> gen_server:reply(From, ok);
                        true -> ok
                    end,
                    {ok, Pid} = supervisor:start_child(Sup, Args),
                    NewRef = erlang:monitor(process, Pid),
                    {noreply, State#state{refs=gb_sets:add(NewRef, NewSet), queue=Q2}}
            end
    end;


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

run(Name, Args)->
    gen_server:call(Name, {add_task, Args}).

sync(Name, Args)->
    gen_server:call(Name, {sync, Args}).

async(Name, Args)->
    gen_server:cast(Name, {async, Args}).


