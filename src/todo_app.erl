-module(todo_app).
-behaviour(application).

-export([start/2, stop/1]). 

start(_StartType, _StartArgs) ->
    io:format("Todo List application started~n"),
    todo_sup:start_link().

stop(_State) ->
    ok.
