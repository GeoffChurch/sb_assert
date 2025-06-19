% Stache :-{ Stash That Actually Can Handle Existentials

:- module(stache, [
   stash/1,
   unstash/1,
   rummage/1,
   rummage_lazy/1
]).

:- use_module(library(chr)).

% TODO we only use these constraints with term_state/2, so it would be more
% efficient to inline, e.g. stash/2, rummage/2, etc.
% But if term expansion cooperates with CHR that would be even better.
:- chr_constraint
   stash/1, % Assume
   unstash/1, % Retract
   rummage/1, % Greedy lookup
   rummage_lazy/1. % Lazy lookup

% Retract a single assumption (uses ==, not =)
unstash(A), stash(A) <=> true.
% unstash(_) <=> fail. % Optionally disallow preemptive retraction.

% Greedy rummage: stack solutions up-front to avoid choice point.
rummage(A), stash(B) <=> unifiable(A,B,_) | (rummage(A) ; A=B), stash(B).
rummage(_)           <=> fail.

% Lazy rummage: lower latency and space usage, but leaves choice point.
rummage_lazy(A), stash(B) <=> (A=B ; rummage_lazy(A)), stash(B).
rummage_lazy(_)           <=> fail.
