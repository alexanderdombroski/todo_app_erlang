-module(todo_main).
-export([start/0]).

start() ->
    io:format("Welcome to the To-Do List Application~n"),
    main_menu().

main_menu() ->
    io:format("Please choose an option:~n"),
    io:format("1. Add a task~n"),
    io:format("2. View tasks~n"),
    io:format("3. Delete a task~n"),
    io:format("4. Exit~n"),
    Input = io:get_line("Enter your choice: "),
    handle_menu_input(string:trim(Input)).

handle_menu_input("1") -> add_task_prompt();
handle_menu_input("2") -> view_tasks_prompt();
handle_menu_input("3") -> delete_task_prompt();
handle_menu_input("4") -> exit(normal);
handle_menu_input(_) -> io:format("Invalid option. Please try again.~n"), main_menu().

add_task_prompt() ->
    Name = io:get_line("Enter task name: "),
    Description = io:get_line("Enter task description: "),
    Status = io:get_line("Enter task status: "),
    DueDate = io:get_line("Enter task due date: "),
    Task = {
        string:trim(Name), 
        string:trim(Description), 
        string:trim(Status), 
        string:trim(DueDate)
    },
    gen_server:call(todo_server, {add_task, Task}),
    main_menu().

view_tasks_prompt() ->
    io:format("------------------------------~n"),
    io:format("----------   Tasks   ---------~n"),
    io:format("------------------------------~n"),
    Tasks = gen_server:call(todo_server, get_tasks),
    lists:foreach(fun display_task/1, Tasks),
    main_menu().

display_task(Task) ->
    {Name, Description, Status, DueDate} = Task,
    io:format("Task Name: ~s~n", [Name]),
    io:format("Task Description: ~s~n", [Description]),
    io:format("Task Status: ~s~n", [Status]),
    io:format("Task Due Date: ~s~n", [DueDate]),
    io:format("------------------------------~n").
    
delete_task_prompt() ->
    Tasks = gen_server:call(todo_server, get_tasks),
    case Tasks of
        [] ->
            io:format("There are no tasks to delete.~n"),
            main_menu();
        _ ->
            display_task_list(Tasks),
            TaskNum = io:get_line("Enter task # to delete: "),
            gen_server:call(todo_server, {delete_task, list_to_integer(string:trim(TaskNum)) - 1}),
            main_menu()
    end.

display_task_list([]) ->
    io:format("There are no tasks.");
display_task_list(Tasks) ->
    io:format("------------------------------~n"),
    display_task_list(Tasks, 1),
    io:format("------------------------------~n").
display_task_list([], _Index) ->
    ok; % Stops Recursion
display_task_list([{Name, _, _, _} | RestTasks], Index) ->
    io:format("~w. ~s~n", [Index, Name]),
    display_task_list(RestTasks, Index + 1).