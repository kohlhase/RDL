:- ensure_loaded(library(lists)).
:- ensure_loaded(base).
:- ensure_loaded(ordering).
:- ensure_loaded(rewrite).

:- multifile method/6.

%%%%%%%%%%%%%%%%%% CONSTRAINT CONTEXTUAL REWRITING %%%%%%%%%%%%%%%%%%%%%%% 

% A literal P can be rewritten to true in rewrite context C if the
% result of extending the context with the negation of the literal
% being rewritten yields an incosistent rewrite context.
method(T-Ex-_,
       cxt_entails_true,
        seq(ccr,       P, true,      C),
       [seq(cs_extend, C,   C1, [NotP])], 
       cs_unsat(T, C1), 
       cxt_entails_true) :- 
	proposition(Ex, P),      % P is a (\Sigma_j, \Pi_c)-literal
        complementary(P, NotP).  % NotP is the negation of P

% Similarly, a literal P can be rewritten to false in rewrite context C
% if the result of extending the context with the literal being
% rewritten yields an incosistent rewrite context. 
method(T-Ex-_,
       cxt_entails_false,
        seq(ccr,       P, false,   C),
       [seq(cs_extend, C,    C1, [P])],
       cs_unsat(T, C1),
       cxt_entails_false) :-
	proposition(Ex,P).       % P is a (\Sigma_j, \Pi_c)-literal

% The activity of normalizing an expression w.r.t. the information
% stored in the rewriting context.
method(_-_-_,
       normal, 
        seq(ccr,       E, E1, C),
       [seq(cs_normal, E, E1, C)],
       true,
       normal).

method(T-Ex-Ord, 
       crew,
         seq(ccr, S, S1, C),
        [seq(rlv, Q, [], C)],
        true,
        crew) :- 
      	rrule(T-Ex,   Q, Lhs, Rhs),
	subexp_at(S,  P, Lhs),
        %%% To avoid the rewriting of propositions 
	\+proposition(Ex,  Lhs),
	%%%
	replace_at(S, P, Rhs, S1),
	ground(S-S1-Q),
%	ordering(Ex, T, Ord),
	msgr(T-Ex-Ord,[S],[S1|Q]).

method(_-_-_,
       crew_rlv,
        seq(rlv, P, P0, C),
       [seq(ccr, E, true, C)],
       true,
       crew_rlv) :-
	select(E, P, P0).







