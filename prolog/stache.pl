:- module(stache, [
	      staching/2,
	      stachings/2,
	      rummage/1,
	      rummage_lazy/1
]).

:- meta_predicate staching(?, 0),
		  stachings(?, 0).

:- use_module(library(chr)).

:- chr_constraint
   % Private constraints
   (+)/1, % Assumption
   (-)/1, % Retraction
   % Run goal given assumption(s)
   staching/2,
   stachings/2,
   % Lookup assumption(s)
   rummage/1,
   rummage_lazy/1.

% Retract a single assumption (uses ==, not =)
-A, +A <=> true.

% Run a goal given assumption(s).
staching(A, G) <=> +A, G, -A.
stachings(As, G) <=> maplist(+, As), G, maplist(-, As).

% Greedy rummage: stack solutions up-front to avoid choice point.
rummage(A), +B <=> unifiable(A,B,_) | (rummage(A) ; A=B), +B.
rummage(_)     <=> fail.

% Lazy rummage: lower latency and space usage, but leaves choice point.
rummage_lazy(A), +B <=> (A=B ; rummage_lazy(A)), +B.
rummage_lazy(_)     <=> fail.
