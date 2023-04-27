-module(nagger).

-behaviour(gen_server).

%% API
-export([start/4, stop/1, start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {n, msg, delay}).

start(Name, Delay, Msg, Times) ->
    gen_server:start({local, Name}, ?MODULE, [Delay, Msg, Times]).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name, Delay, Msg, Times) ->
    gen_server:start_link({local, Name}, ?MODULE, [Delay, Msg, Times], []).

init([Delay, Msg, Times]) ->
    self() ! {nag},
    {ok, #state{delay=Delay, msg=Msg, n=Times}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    self() ! {nag},
    {noreply, State};

handle_info({nag}, State=#state{delay=Delay, msg=Msg, n=Times}) when Times > 0 ->
    io:format("Hey listen ! Got a msg [~p]~n", [Msg]),
    {noreply, State#state{n=Times-1}, Delay};

handle_info({nag}, State=#state{ n=Times}) when Times =< 0 ->
    io:format("Task done!~n"),
    {stop, normal, State};

handle_info(_Info, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
