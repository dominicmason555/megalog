% Print list of todos
print_list(_, []) :- writeln("").
print_list(Fstring, [H|T]) :- format(Fstring, H), print_list(Fstring, T).

% Get Todos from facts
todo_exists(O) :- fact(_, _, _, "todo", O).
todo_new(O) :- fact(_, _, "new", "todo", O).
todo_did(O) :- fact(_, _, "did", "todo", O).
todo_cancelled(O) :- fact(_, _, "cancelled", "todo", O).

% Todos which were created more times than they were done or cancelled
todo_active(O) :-
    findall(O, todo_new(O), News), length(News, NumNews),
    findall(O, todo_did(O), Dids), length(Dids, NumDids),
    findall(O, todo_cancelled(O), Cancelleds), length(Cancelleds, NumCancelleds),
    NumNews > (NumDids+NumCancelleds).

% All Todos
td(Os) :- findall(O, todo_exists(O), Os).

% All active Todos
tda(Os) :- setof(O, (todo_exists(O), todo_active(O)), Os).

% Print all active Todos
print_active() :- tda(Ts), print_list("Todo: ~s\n", Ts).

% Format today as a string
day_string(S) :- get_time(T), format_time(string(S), "%Y-%m-%d %A of Week %W", T).

% Get all unique days used as a date
day_set(Days) :- findall(D, fact(_, _, "date", "day", D), Ds), setof(Day, member(Day, Ds), Days).

% Get number of unique days used as a date
day_count(Numdays) :- day_set(Days), length(Days, Numdays).

% Entry to interactive query
query_entry() :-
    day_string(Day),
    day_count(Numdays),
    format("# ~s\n\n", Day),
    format("~d Days Logged\n\n", Numdays),
    format("## Active Todos:\n\n"),
    print_active().
