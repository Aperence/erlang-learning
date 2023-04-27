-module(erlcounter).

-export([start/2, stop/1]).

start(normal, _Args)->
    {ok, Dir} = application:get_env(directory),
    {ok, Regex} = application:get_env(regex),
    {ok, Limit} = application:get_env(maxfiles),
    {ok, Pid} = erlcounter_dispatcher:start_link(Dir, Limit, Regex),
    {ok, Res} = erlcounter_dispatcher:launch_count(Pid),
    lists:foreach(fun (X) -> 
        {Re, Count} = X,
        io:format("Number of occurrences for ~p : ~p~n", [Re, Count]) 
    end, Res),
    {ok, Pid}.


stop(_Args)->
    ok.