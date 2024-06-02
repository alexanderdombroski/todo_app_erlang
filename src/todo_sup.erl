
-module(todo_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = {one_for_one, 1, 5},
    ChildSpecs = [
        {
            todo_server,
            {todo_server, start_link, []},
            permanent,
            5000,
            worker,
            [todo_server]
        }
    ],
    {ok, {RestartStrategy, ChildSpecs}}.