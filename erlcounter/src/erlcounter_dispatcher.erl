-module(erlcounter_dispatcher).

-behaviour(gen_statem).

-export([stop/0, start_link/3, complete/4, launch_count/1]).
-export([init/1, callback_mode/0, dispatching/3, terminate/3, code_change/4, waiting_done/3]).

-record(state, {
    caller,
    refs,
    dirs,
    regex,
    counts
}).

launch_count(Pid)->
    gen_statem:call(Pid, start_count).

complete(Name, File, Ref, Counts) ->
    gen_statem:cast(Name, {counts, File, Ref, Counts}).

stop() ->
    gen_statem:stop(?MODULE).

start_link(Dir, Limit, Regex) ->
    gen_statem:start_link(?MODULE, [Dir, Limit, Regex], []).

init([Dir, Limit, Regex]) ->
    ppool:start_pool(erlcounter, {counter_worker, start_link, []}, Limit),
    Counts = [{Re, 0} || Re <- Regex],
    {ok, dispatching, #state{refs=[], counts=Counts, dirs=library_counter:start(Dir), regex=Regex}}.

%% state_functions | handle_event_function | [_, state_enter].
callback_mode() ->
    state_functions.

dispatching({call,From}, start_count, Data) -> 
    gen_statem:cast(self(), handle),
    {next_state, dispatching ,Data#state{caller=From}};

dispatching(cast, {counts, _WorkerPid, Ref, CountsFile}, Data=#state{refs=Refs, counts=Counts})->
    NewRefs = lists:delete(Ref, Refs),
    NewCounts = [{Re, X + Y} || {Re, X} <- Counts, {Re2, Y} <- CountsFile, Re==Re2],
    {next_state, dispatching, Data#state{refs=NewRefs, counts=NewCounts}};

dispatching(cast, handle, Data=#state{refs=Refs, dirs=Dir, regex=Regex}) -> 
    case Dir of 
        done ->
            waiting_done(cast, result, Data);  % might not receive a message anymore, so call it directly
        {continue, File, F} ->
            Ref=make_ref(),
            ppool:async(erlcounter, [File, self(), Ref, Regex]),
            gen_statem:cast(self(), handle),
            {next_state, dispatching, Data#state{dirs=F(), refs=Refs ++ [Ref]}}
    end.

waiting_done(cast, {counts, _FileTreated, Ref, CountsFile}, Data=#state{refs=Refs, counts=Counts})->
    NewRefs = lists:delete(Ref, Refs),
    NewCounts = [{Re, X + Y} || {Re, X} <- Counts, {Re2, Y} <- CountsFile, Re==Re2],
    gen_statem:cast(self(), result),
    {next_state, waiting_done, Data#state{refs=NewRefs, counts=NewCounts}};

waiting_done(cast, result, _Data=#state{caller=Pid, refs=Refs, counts=Counts}) when Refs =:= [] ->
    gen_statem:reply(Pid, {ok, Counts}),
    {stop, normal};

waiting_done(cast, result, Data) ->
    {next_state, waiting_done, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
