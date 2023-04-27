-module(supervisor_workers).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([init/1]).

start_link(MFA) ->
    supervisor:start_link(?MODULE, [MFA]).

init([MFA={M, _F, _A}]) ->
    SupervisorSpecification = #{
        strategy => simple_one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 1,
        period => 60},

    ChildSpecifications = [
        #{
            id => worker,
            start => MFA,
            restart => temporary, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [M]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
