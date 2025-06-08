:- module(sb_assert, [
         sb_assertz/2,
         sb_retract/1
]).

:- use_module(library(chr)).

%%% BEGIN CHR

:- chr_constraint
   (+)/1, % Assume
   (-)/1, % Retract
   rummage/1,
   rummage_lazy/1.

% Retract a single assumption (uses ==, not =)
-A, +A <=> true.
% -_ <=> fail. % Optional: don't leave retractions floating around (easier to reason about but precludes preemptive retraction).

% Greedy rummage: stack solutions up-front to avoid choice point.
rummage(A), +B <=> unifiable(A,B,_) | (rummage(A) ; A=B), +B.
rummage(_)     <=> fail.

% Lazy rummage: lower latency and space usage, but leaves choice point.
rummage_lazy(A), +B <=> (A=B ; rummage_lazy(A)), +B.
rummage_lazy(_)     <=> fail.

%%% END CHR


:- meta_predicate sb_assertz(?, :).
sb_assertz(State, M:Term) =>
    $(nonvar(Term)),
    term_head_body(Term, H, B),
    setup_wrapper(M, (H :- B)),
    % TODO get rid of (+), just use term_state, and maybe specialize rummage.
    +term_state((H :- B), State).

term_head_body(H :- B, H_, B_) :- !, H_ = H, B_ = B.
term_head_body(Term, H, B) :- H = Term, B = true.


setup_wrapper(M, (H :- _B)) =>
    functor(H, F, A),
    setup_wrapper_(M, F/A).

:- dynamic wrapper_ref/3.
setup_wrapper_(M, F/A) =>
    (wrapper_ref(F/A, M, _)
    ->  true
    ;   functor(H, F, A),
        b_assertz(M:(H :- sb_assert:eval(H)), Ref),
	b_assertz(sb_assert:wrapper_ref(F/A, M, Ref), _)
    ).

eval(H) :-
    same_functor(H, H_),
    rummage(term_state((H_ :- B_), State)),
    copy_term((H_ :- B_)-State, (H :- B)-State),
    call(B).

term_headfunctor(Term, F/A) :-
    Term = (H :- _)
    ->  functor(H, F, A)
    ;   functor(Term, F, A).

:- det(b_assertz/2).
b_assertz(M:Term, Ref) :-
    $(M:assertz(Term, Ref)),
    undo(erase(Ref)).

sb_retract(Term) :-
    rummage(term_state(Term, State)),
    -term_state(Term, State),
    same_functor(Term, H),
    (rummage_lazy(term_state(H, _))
    ->  true
    ;   term_headfunctor(Term, F/A),
	wrapper_ref(F/A, Ref),
	erase(Ref)
    ).
	
    

    % retract(wrapper_ref(F/A, Ref)),
    % erase(Ref).

sb_retractall(H) :-
    (sb_retract(H)
    ->  sb_retractall_(H)
    ;   true).
