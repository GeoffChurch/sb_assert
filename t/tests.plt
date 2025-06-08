:- use_module(library(sb_assert)).

:- begin_tests(sb_assert).

:- use_module(library(chr)).

test(starts_empty, fail) :-
    sb_retract(_).

test(simple) :-
    maplist(sb_assertz([]), [a, b, c, d, e]),
    a, c, e, d, a.

test(simple_fail, error(existence_error(procedure,_M:f/0))) :-
    maplist(sb_assertz([]), [a, b, c, d, e]),
    b, d, f.

test(multiple_assumptions) :-
    sb_assertz(A, f(A)),
    sb_assertz([], g),
    sb_assertz(B, f(B)),
    sb_assertz(C, f(C)),
    f(X),
    (X == A ; X == B ; X == C),
    X \== A,
    X \== B,
    X == C.

test(combinations, all(X-Y == [a-c, a-d, b-c, b-d])) :-
    maplist(sb_assertz([]), [f(a), f(b), g(c), g(d)]),
    f(X),
    g(Y).

test(persistence, [X == Y]) :-
    sb_assertz(X, f(X)),
    f(Y).

:- end_tests(sb_assert).

:- run_tests.
