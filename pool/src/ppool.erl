
-module(ppool).
-behaviour(supervisor).

%% API
-export([start/2, stop/1, start_pool/3, main/0, run/2, sync/2, async/2]).
-export([init/1]).

-define(WORKER(Name, MFA, Limit), 
    #{
    id => ppool_supervisor,
    start => {ppool_supervisor, start_link, [Name, MFA, Limit]},
    restart => transient, % permanent | transient | temporary
    shutdown => 2000,
    type => supervisor, % worker | supervisor
    modules => [ppool_supervisor]
    }
).

start(normal, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    ChildSpecifications = [],

    {ok, {SupervisorSpecification, ChildSpecifications}}.

start_pool(Name, MFA, Limit)->
    supervisor:start_child(ppool, ?WORKER(Name, MFA, Limit)).

stop(_State)->
    ok.

run(Name, Args)->
    pool_server:run(Name, Args).

sync(Name, Args)->
    pool_server:sync(Name, Args).

async(Name, Args)->
    pool_server:async(Name, Args).

main()->
    application:start(ppool),
    ppool:start_pool(naggers, {nagger, start_link, []}, 2),
    ppool:run(naggers, [nagger1, 2000, "Hello", 2]),
    ppool:async(naggers, [nagger2, 1000, "Hey", 3]),
    ppool:sync(naggers, [nagger3, 3000, "Hi", 2]),
    spawn(fun() -> 
        timer:sleep(7000), 
        application:stop(ppool) end),
    ok.
