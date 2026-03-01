:- use_module(library(http/html_write)).

% Print list
print_list(_, []) :- writeln("").
print_list(Fstring, [H|T]) :- format(Fstring, H), print_list(Fstring, T).

% Print ordered markdown list
print_ordered_list(_, _, []) :- writeln("").
print_ordered_list(Fstring, Num, [H|T]) :- format(Fstring, [Num|H]), Inc=Num+1, print_ordered_list(Fstring, Inc, T).

dated(S, ID, D) :- fact(S, ID, "Date", "Day", D).

rated_percent(S, ID, O) :- fact(S, ID, "Rate", "%", O).

type_alias_dated_rated(A, S, Id, V, T, F, R, D) :-
    fact(S, Id, V, T, F),
    type_alias(T, A),
    rated_percent(S, Id, R),
    dated(S, Id, D).

type_ranked(Type) :-
   findall([R,D,V,T,F,S], (type_alias_dated_rated(Type, _, _, V, T, F, R, D), all_view_url_markdown(T,F,S)), Rs),
   sort(0, @>, Rs, Sorted),
   length(Rs, Num),
   format("## Rated ~d ~ss:~n~n", [Num, Type]),
   print_ordered_list("~d. ~s% ~s: ~s ~s ~s~s~n", 1, Sorted).

type_alias(T, T).
type_alias(T, A) :-
    fact(S, I, "NewAlias", "Type", A),
    fact(S, I, "AddAlias", "Type", T).

url_prefix_type(T, U) :-
    fact(S, I, "NewURLType", "Type", T),
    fact(S, I, "AddViewURL", "URLPrefix", U).

view_url_from_id(S, I, Url) :-
    fact(S, I, _, T, O),
    type_alias(T, A),
    url_prefix_type(A, U),
    string_concat(U, O, Url).

view_url_from_type(A, O, Url) :-
    type_alias(T, A),
    fact(_, _, _, T, O),
    fact(S, I, _, _, O),
    fact(S, I, _, IDType, IDO),
    url_prefix_type(IDType, U),
    string_concat(U, IDO, Url).

view_url_from_type_md(A, O, Url) :-
    type_alias(T, A),
    fact(_, _, _, T, O),
    fact(S, I, _, _, O),
    fact(S, I, _, IDType, IDO),
    url_prefix_type(IDType, U),
    format(string(Url), "[~s](~s~s)", [IDType, U, IDO]).

all_view_url_from_type_string(T, O, S) :-
    findall(U, view_url_from_type(T, O, U), Us),
    with_output_to(string(S), maplist(format(" ~s"), Us)).

all_view_url_markdown(T, O, S) :-
    findall(U, view_url_from_type_md(T, O, U), Us),
    sort(Us, Sorted),
    with_output_to(string(S), maplist(format(" ~s"), Sorted)).

% Get Todos from facts
todo_exists(O) :- fact(_, _, _, "Todo", O).
todo_new(O) :- fact(_, _, "New", "Todo", O).
todo_did(O) :- fact(_, _, "Did", "Todo", O).
todo_cancelled(O) :- fact(_, _, "Cancelled", "Todo", O).

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
    fact(S,L,"New","Todo",T),
    fact(S,L,"Date","Day",D),
    todo_active(T).

% Active todos sorted by date
todo_all_active(S) :-
    findall([Created,Todo],
        todo_active_date(Todo,Created), Ts),
    sort(Ts,S).

goal_exists(G) :- fact(_, _, "New", "Goal", G).

goal_start_date(G, D) :-
    fact(S, I, "New", "Goal", G),
    fact(S, I, "Date", "Day", D).

goal_end_date(G, D) :-
    fact(S, I, "New", "Goal", G),
    fact(S, I, "EndDate", "Day", D).

goal_start_end(G, S, E) :- goal_start_date(G, S), goal_end_date(G, E).

goal_active_as_of(Goal, Day) :-
    goal_start_end(Goal, StartDate, EndDate),
    StartDate @=< Day,
    EndDate @>= Day.

goal_active_today(Goal) :-
    get_time(Time),
    format_time(string(Today), "%Y-%m-%d", Time),
    goal_active_as_of(Goal, Today).

goal_did_active(Goal) :-
    fact(S, I, "Did", "Goal", Goal),
    fact(S, I, "Date", "Day", Day),
    goal_active_as_of(Goal, Day).

goal_did_times(Goal, NumTimes) :-
    findall(Goal, goal_did_active(Goal), Times),
    length(Times, NumTimes).

goal_already_done_times(G, T) :-
    fact(S, I, "New", "Goal", G),
    fact(S, I, "Done", "Times", N),
    atom_number(N, T).

goal_total_done_times(Goal, Times) :-
  goal_already_done_times(Goal, DoneTimes) ->
    goal_did_times(Goal, DidTimes),
    Times is DidTimes + DoneTimes;
  goal_did_times(Goal, Times).

zip4([], [], [], [], []).
zip4([A | As], [B | Bs], [C | Cs], [D | Ds], [[A, B, C, D] | ABCDs]) :- zip4(As, Bs, Cs, Ds, ABCDs).

goals_times(UniqueGoals, GT) :-
    maplist(goal_start_date, UniqueGoals, StartDates),
    maplist(goal_end_date, UniqueGoals, EndDates),
    maplist(goal_total_done_times, UniqueGoals, DoneTimes),
    zip4(StartDates, EndDates, UniqueGoals, DoneTimes, GT).

all_goals_times(GT) :-
    setof(Goal, goal_active_today(Goal), UniqueGoals),
    goals_times(UniqueGoals, GT).

active_today_goals_times(GT) :-
    setof(Goal, goal_exists(Goal), UniqueGoals),
    goals_times(UniqueGoals, GT).

print_all_goals() :-
    all_goals_times(GTs),
    length(GTs, NumGTs),
    format("## ~d Goals:\n\n", NumGTs),
    print_ordered_list("~d. ~w - ~w: `~w` Done ~d Times~n", 1, GTs).

print_today_goals() :-
    active_today_goals_times(GTs),
    length(GTs, NumGTs),
    format("## ~d Active Goals:\n\n", NumGTs),
    print_ordered_list("~d. ~w - ~w: `~w` Done ~d Times~n", 1, GTs).

% Print all active Todos
print_active() :-
    todo_all_active(Ts),
    length(Ts, NumTs),
    format("## ~d Active Todos:\n\n", NumTs),
    print_ordered_list("~d. ~w: `~w`~n", 1, Ts).

% Format today as a string
day_string(S) :- get_time(T), format_time(string(S), "%Y-%m-%d %A of Week %W", T).

% Get all unique days used as a date
day_set(Days) :- findall(D, fact(_, _, "Date", "Day", D), Ds), setof(Day, member(Day, Ds), Days).

% Get number of unique days used as a date
day_count(Numdays) :- day_set(Days), length(Days, Numdays).

fact_count(Numfacts) :- findall(I, fact(_,I,_,_,_), Is), length(Is, Numfacts).

triple_html([Rel, Type, Object]) :-
    phrase(html([
        fact_triple([
            fact_rel(Rel), " ", fact_type(Type), " ", fact_object(Object)
        ]), "\n"]),
    H),
    print_html(H).

fact_group_html([_, _, _ | RTO]) :-
  writeln("<fact_group>"),
  maplist(triple_html, RTO),
  writeln("</fact_group>"),
  writeln("<hr>").

facts_html() :- facts_grouped(Facts), maplist(fact_group_html, Facts).

id_set(Ids) :- findall(AllId, fact(_, AllId, _, _, _), AllIds), setof(Id, member(Id, AllIds), Ids).

facts_from_ids([], Accum, Facts) :- sort(Accum, Facts).
facts_from_ids([Id | Ids], Accum, Facts) :-
    findall([V, T, O], fact(_, Id, V, T, O), IdFacts),
    split_string(Id, ":", "", [LineStr, NumStr]),
    atom_number(LineStr, Line),
    atom_number(NumStr, Number),
    facts_from_ids(Ids, [[Line, Number, Id | IdFacts] | Accum], Facts).

facts_grouped(Facts) :-
    id_set(Ids),
    facts_from_ids(Ids, [], Facts).

facts_grouped_to_file(Filename):-
    facts_grouped(Facts),
    open(Filename, write, Out),
    write_canonical(Out, Facts),
    close(Out).

facts_grouped_to_html_file(Filename):-
    open(Filename, write, Out),
    with_output_to(Out, facts_html()),
    % write_canonical(Out, Facts),
    close(Out).

% Entry to interactive query
query_entry() :-
    day_string(Day),
    day_count(Numdays),
    fact_count(Numfacts),
    format("# ~s~n", Day),
    format("~d Facts from ~d Days Logged, ~1f Facts per Day~n", [Numfacts, Numdays, Numfacts/Numdays]),
    print_active,
    print_today_goals.
