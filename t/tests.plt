:- use_module(library(stache)).

:- begin_tests(stache, [cleanup(empty)]).

empty :- $(\+rummage(_)).

rummages(As) :- maplist(rummage, As).
rummages_lazy(As) :- maplist(rummage_lazy, As).

test(starts_empty) :-
    empty.

test(ends_empty_on_success) :-
    staching(a, stachings([b,c], stachings([d,e], rummages([a,c,e,d,a])))),
    empty.

test(ends_empty_on_success_lazy, [nondet]) :-
    staching(a, stachings([b,c], stachings([d,e], rummages_lazy([a,c,e,d,a])))),
    empty.

test(ends_empty_on_failure) :-
    \+ staching(a, stachings([b,c], stachings([d,e], rummages([b,d,f])))),
    empty.

test(ends_empty_on_failure_lazy) :-
    \+ staching(a, stachings([b,c], stachings([d,e], rummages_lazy([b,d,f])))),
    empty.

test(multiple_assumptions, all(X == [a, b, c])) :-
    staching(f(a), staching(g, stachings([f(b),f(c)], rummage(f(X))))).

test(multiple_assumptions_lazy, all(X == [c, b, a])) :-
    staching(f(a), staching(g, stachings([f(b),f(c)], rummage_lazy(f(X))))).

test(combinations, all(X-Y == [a-c, a-d, b-c, b-d])) :-
    stachings([f(a),f(b),g(c),g(d)],
	      rummages([f(X),g(Y)])).

test(combinations_lazy, all(X-Y == [b-d, b-c, a-d, a-c])) :-
    stachings([f(a),f(b),g(c),g(d)],
	      rummages_lazy([f(X),g(Y)])).

test(persistence) :-
    staching(f(X), rummage(f(Y))),
    X == Y.

test(persistence_lazy, [nondet]) :-
    staching(f(X), rummage_lazy(f(Y))),
    X == Y.

:- end_tests(stache).
