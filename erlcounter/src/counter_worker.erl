-module(counter_worker).

-behaviour(gen_server).

%% API
-export([start/4, stop/1, start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {filename, parent, ref, regex}).


start(Filename, Pid, Ref, Regex) ->
    gen_server:start(?MODULE, [Filename, Pid, Ref, Regex]).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Filename, Pid, Ref, Regex) ->
    gen_server:start_link(?MODULE, [Filename, Pid, Ref, Regex], []).

init([Filename, Pid, Ref, Regex]) ->
    self() ! handle_file,
    {ok, #state{filename=Filename, parent=Pid, ref=Ref, regex=Regex}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(handle_file, State=#state{filename=Filename, regex=Regex, parent=Pid, ref=Ref}) ->
    {ok, Text} = file:read_file(Filename),
    Counts = [{Re, library_counter:regex_number_matchs(Text, Re)} || Re <- Regex],
    erlcounter_dispatcher:complete(Pid, Filename, Ref, Counts),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
