-module(library_counter).

-include_lib("kernel/include/file.hrl").

-export([start/1, regex_number_matchs/2]).

start(Name) ->
    loop_files(Name, queue:new()).

% Progressive continuation
% We split a recursive function in successive calls,
% which we might break between the different calls

loop_files(Name, Queue)->
    {ok, F} = file:read_file_info(Name),
    case F#file_info.type of 
        directory ->
            handle_directory(Name, Queue);
        regular ->
            handle_file(Name, Queue);
        _Other ->
            dequeue_run(Queue)
    end.

handle_directory(Name, Queue)->
    {ok, Files} = file:list_dir(Name),
    NewQueue = lists:foldl(fun (File, Acc) -> queue:in(filename:join(Name, File), Acc) end, Queue, Files),
    dequeue_run(NewQueue).  % continue the search

handle_file(Name, Queue)->
    case filename:extension(Name) of 
        ".erl" ->
            {continue, Name, fun () -> dequeue_run(Queue) end};  % a result is found
        _ -> dequeue_run(Queue)  % No result => continue the search
    end.


dequeue_run(Queue)->
    case queue:out(Queue) of 
        {empty, _} -> 
            done;  % No more result
        {{value, N}, Rem} ->
            loop_files(N, Rem)
    end.

regex_number_matchs(String, Pattern)->
    case re:run(String, Pattern, [global]) of 
        nomatch -> 0;
        {match, L} -> length(L)
    end.