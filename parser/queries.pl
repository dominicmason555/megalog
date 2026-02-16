% Print list
print_list(_, []) :- writeln("").
print_list(Fstring, [H|T]) :- format(Fstring, H), print_list(Fstring, T).

% Print ordered markdown list
print_ordered_list(_, _, []) :- writeln("").
print_ordered_list(Fstring, Num, [H|T]) :- format(Fstring, [Num|H]), Inc=Num+1, print_ordered_list(Fstring, Inc, T).

dated(S, ID, D) :- fact(S, ID, "date", "day", D).

rated_percent(S, ID, O) :- fact(S, ID, "rate", "%", O).

food(S, ID, V, T, O) :-
    fact(S, ID, V, T, O),
    (T="breakfast"; T="lunch"; T="dinner"; T="pud"; T="snack").


food_dated_rated(S, Id, V, T, F, R, D) :- food(S, Id, V, T, F), rated_percent(S, Id, R), dated(S, Id, D).

food_ranked() :-
  findall([R,D,V,T,F], food_dated_rated(_, _, V, T, F, R, D), Rs),
  sort(Rs, Sorted),
  length(Rs, NumFoods),
  format("Rated ~d meals:~n~n", [NumFoods]),
  print_ordered_list("~d. ~s% ~s: ~s ~s ~s~n", 1, Sorted).

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

% Active Todos and their creation dates
todo_active_date(T, D) :-
    fact(S,L,"new","todo",T),
    fact(S,L,"date","day",D),
    todo_active(T).

% Active todos sorted by date
todo_all_active(S) :-
    findall([Created,Todo],
        todo_active_date(Todo,Created), Ts),
    sort(Ts,S).

% Print all active Todos
print_active() :-
    todo_all_active(Ts),
    length(Ts, NumTs),
    format("## ~d Active Todos:\n\n", NumTs),
    print_ordered_list("~d. ~w: `~w`~n", 1, Ts).

% Format today as a string
day_string(S) :- get_time(T), format_time(string(S), "%Y-%m-%d %A of Week %W", T).

% Get all unique days used as a date
day_set(Days) :- findall(D, fact(_, _, "date", "day", D), Ds), setof(Day, member(Day, Ds), Days).

% Get number of unique days used as a date
day_count(Numdays) :- day_set(Days), length(Days, Numdays).

fact_count(Numfacts) :- findall(I, fact(_,I,_,_,_), Is), length(Is, Numfacts).

% Entry to interactive query
query_entry() :-
    day_string(Day),
    day_count(Numdays),
    fact_count(Numfacts),
    format("# ~s~n", Day),
    format("~d Facts from ~d Days Logged, ~1f Facts per Day~n", [Numfacts, Numdays, Numfacts/Numdays]),
    print_active().
