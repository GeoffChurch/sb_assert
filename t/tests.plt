:- use_module(library(stache)).

:- begin_tests(stache).

:- use_module(library(chr)).

rummages(As) :- maplist(rummage_, As).
rummages_lazy(As) :- maplist(rummage_lazy_, As).

rummage_(A) :- writeln(rummage_(A)), rummage(A).
rummage_lazy_(A) :- writeln(rummage_lazy_(A)), rummage_lazy(A).

test(starts_empty, fail) :-
    rummage(_).

test(simple) :-
    maplist(+, [a, b, c, d, e]),
    rummages([a, c, e, d, a]).

test(simple_lazy, [nondet]) :-
    maplist(+, [a, b, c, d, e]),
    rummages_lazy([a, c, e, d, a]).

test(simple_fail) :-
    maplist(+, [a, b, c, d, e]),
    rummage(b),
    rummage(d),
    \+ rummage(f),
    rummage(d).

test(simple_fail_lazy, [nondet]) :-
    maplist(+, [a, b, c, d, e]),
    rummage_lazy(b),
    rummage_lazy(d),
    \+ rummage_lazy(f),
    rummage_lazy(d).

test(multiple_assumptions) :-
    maplist(+, [f(A), g, f(B), f(C)]),
    rummage(f(X)),
    (X == A ; X == B ; X == C),
    X \== A,
    X \== B,
    X == C.

test(multiple_assumptions_lazy, all(X == [c, b, a])) :-
    maplist(+, [f(a), g, f(b), f(c)]),
    rummage_lazy(f(X)).

test(combinations, all(X-Y == [a-c, a-d, b-c, b-d])) :-
    maplist(+, [f(a), f(b), g(c), g(d)]),
    rummages([f(X), g(Y)]).

test(combinations_lazy, all(X-Y == [b-d, b-c, a-d, a-c])) :-
    maplist(+, [f(a), f(b), g(c), g(d)]),
    rummages_lazy([f(X), g(Y)]).

test(persistence, [X == Y]) :-
    +f(X),
    rummage(f(Y)).

test(persistence_lazy, [nondet, X == Y]) :-
    +f(X),
    rummage_lazy(f(Y)).

:- end_tests(stache).

:- run_tests.