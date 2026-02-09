% Todos which aren't done or cancelled.
todo(O) :-
    (fact(_,_,"new","todo",O),
     \+fact(_,_,"did","todo",O),
     \+fact(_,_,"cancelled","todo",O)).

td(Os) :- findall(O, todo(O), Os).
