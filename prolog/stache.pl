:- module(stache, [
         (+)/1,
         (-)/1,
	      rummage/1,
	      rummage_lazy/1
]).

:- use_module(library(chr)).

:- chr_constraint
   % Private constraints
   (+)/1, % Assumption
   (-)/1, % Retraction
   % Lookup assumption(s)
   rummage/1,
   rummage_lazy/1.

% Retract a single assumption (uses ==, not =)
-A, +A <=> true.%, writeln((-A, +A)).
% -_ <=> fail. % Optional: don't leave retractions floating around (easier to reason about but precludes preemptive retraction).

% Greedy rummage: stack solutions up-front to avoid choice point.
rummage(A), +B <=> unifiable(A,B,_) | (rummage(A) ; A=B), +B.
rummage(_)     <=> fail.

% Lazy rummage: lower latency and space usage, but leaves choice point.
rummage_lazy(A), +B <=> (A=B ; rummage_lazy(A)), +B.
rummage_lazy(_)     <=> fail.
