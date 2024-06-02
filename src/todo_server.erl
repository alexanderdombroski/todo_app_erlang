-module(todo_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tasks}).

-define(TODO_FILE, "todo_list.csv").

start_link() ->
    io:format("Starting todo_server...~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case read_tasks_from_file() of
        {ok, Tasks} ->
            {ok, #state{tasks = Tasks}};
        {error, Error} ->
            io:write(Error),
            {error, Error}
    end.

handle_call({add_task, Task}, _From, State) ->
    NewTasks = [Task | State#state.tasks],
    save_tasks_to_file(NewTasks),
    {reply, ok, State#state{tasks = NewTasks}};
handle_call(get_tasks, _From, State) ->
    {reply, State#state.tasks, State};
handle_call({delete_task, TaskNum}, _From, State) ->
    Tasks = State#state.tasks,
    case delete_task_by_index(TaskNum, Tasks) of
        {ok, NewTasks} ->
            save_tasks_to_file(NewTasks),
            {reply, ok, State#state{tasks = NewTasks}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

save_tasks_to_file(Tasks) ->
    % Open the file
    {ok, File} = file:open(?TODO_FILE, [write]),
    % Iterate over each task and write it to the file
    lists:foreach(fun(Task) ->
        % Format the task as a CSV line
        Line = format_task_as_csv(Task),
        % Write the CSV line to the file
        ok = file:write(File, Line ++ "\n")
    end, Tasks),
    % Close the file
    file:close(File).

read_tasks_from_file() ->
    % Open the file for reading
    case file:open(?TODO_FILE, [read]) of
        {ok, File} ->
            % Read each line from the file and parse it as a task
            Tasks = read_tasks_from_file(File, []),
            % Close the file
            file:close(File),
            {ok, Tasks};
        {error, _} ->
            % If the file does not exist or cannot be opened, return an empty list
            {error, unknown_error}
    end.

read_tasks_from_file(File, Acc) ->
    % Read a line from the file
    case file:read_line(File) of
        {ok, Line} ->
            % Trim the line to remove any extra newlines or spaces
            TrimmedLine = string:trim(Line),
            % Skip empty lines
            case TrimmedLine of
                "" ->
                    read_tasks_from_file(File, Acc);
                _ ->
                    % Parse the CSV line as a task
                    case parse_task_from_csv(TrimmedLine) of
                        {error, _} ->
                            read_tasks_from_file(File, Acc);
                        Task ->
                            read_tasks_from_file(File, [Task | Acc])
                    end
            end;
        eof -> % eof = end-of-file
            % If end-of-file is reached, reverse the accumulator and return the list of tasks
            lists:reverse(Acc)
    end.

format_task_as_csv(Task) ->
    TaskData = [element(N, Task) || N <- lists:seq(1, tuple_size(Task))],
    string:join(TaskData, ",").

parse_task_from_csv(Line) ->
    case string:tokens(Line, ",") of
        [Name, Description, Status, DueDate] ->
            {Name, Description, Status, DueDate};
        _ ->
            {error, invalid_csv_line}
    end.

delete_task_by_index(Index, Tasks) ->
    case lists:split(Index, Tasks) of
        {Before, [_|After]} ->
            {ok, Before ++ After};
        _ ->
            {error, index_out_of_bounds}
    end.


% These aren't needed but can be left as stubs in case needed in future
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.