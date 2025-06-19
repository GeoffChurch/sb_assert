:- module(sb_assert, [
    sb_assertz/2,
    sb_retract/1,
    sb_retractall/1
]).

:- use_module(library(stache)).

:- dynamic wrapper_ref/3.

% Stateful, backtrackable assertz.
:- meta_predicate sb_assertz(?, :).
sb_assertz(State, M:Term) =>
    $(nonvar(Term)),
    $(term_head_body(Term, H, B)),
    $(setup_wrapper(M, (H :- B))),
    $(stash(term_state((H :- B), State))).

sb_retract(Term) :-
    rummage(term_state(Term, State)),
    $(unstash(term_state(Term, State))),
    $(same_functor(Term, H)),
    (rummage_lazy(term_state(H, _))
    ->  true
    ;   $(term_headfunctor(Term, F/A)),
        $(retract(wrapper_ref(F/A, Ref))),
        $(erase(Ref))).

sb_retractall(H) :-
    $(copy_term(H, H_)),
    (sb_retract(H_)
    ->  sb_retractall(H)
    ;   true).

term_head_body(T, H, B), nonvar(T) =>
    T = (H_ :- B_)
    ->  H = H_, B = B_
    ;   H = T, B = true.

setup_wrapper(M, (H :- _B)) =>
    functor(H, F, A),
    setup_wrapper_(M, F/A).

setup_wrapper_(M, F/A) =>
    wrapper_ref(F/A, M, _)
    ->  true
    ;   functor(H, F, A),
        b_assertz(M:(H :- sb_assert:eval(H)), Ref),
        b_assertz(wrapper_ref(F/A, M, Ref), _).

eval(H) :-
    $(same_functor(H, H_)),
    rummage(term_state((H_ :- B_), State)),
    copy_term((H_ :- B_)-State, (H :- B)-State),
    call(B).

term_headfunctor(Term, F/A) :-
    Term = (H :- _)
    ->  $(functor(H, F, A))
    ;   $(functor(Term, F, A)).

:- det(b_assertz/2).
b_assertz(Term, Ref) =>
    $(assertz(Term, Ref)),
    undo(erase(Ref)).
