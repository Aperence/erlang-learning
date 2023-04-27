-module(ppool_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/3]).
-export([init/1]).

start_link(Name, MFA, Limit) ->
    supervisor:start_link(?MODULE, [Name, MFA, Limit]).

init([Name, MFA, Limit]) ->
    SupervisorSpecification = #{
        strategy => one_for_all, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 5,
        period => 60},

    ChildSpecifications = [
        #{
            id => server,
            start => {pool_server, start_link, [Name, self(), MFA, Limit]},
            restart => transient, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [pool_server]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
