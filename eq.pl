:- ensure_loaded(library(lists)).
:- ensure_loaded(library(ordsets)).

:- ensure_loaded(base).
:- ensure_loaded(reduce).
:- ensure_loaded(simplify).
:- ensure_loaded(ccr).
:- ensure_loaded(cs_extend).
:- ensure_loaded(augment).

:- use_module(shostak).

:- multifile method/6, cs_init_state/2, cs_unsat/2, gls/7, arrow/5.

%%% Interface to the constraint solver %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cs_init_state(T,[]-[]-Sh0) :- 
	(T =..[_,eq]; T =eq),
	cc_init(Sh0).
cs_unsat(T,_-A-_)          :- 
	(T =..[_,eq]; T =eq),
	unsat(A).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% Interface method to ccr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
method(T-Ex-Ord, 
       normal, 
        seq(cs_normal, E, E1, _-_-Sh), 
       [], 
       true, 
       cs_canon) :- 
	(T =..[_,eq]; T =eq),
	cc_canon(Sh, E, E1),
	ord(T-Ex-Ord,E,E1,gr).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Constraint simplification %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   seq(cs_simp, B-A-Sh, B1-A1-Sh1, Ps)
%     -  B: is the set of fact currently being added by augment
%     -  A: is a set of literals
%     - Sh: the state of the Shostak congruence closure algorithm
%     - Ps: input literals 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Methods for constraint simplification %%%%%%%%%%%%%%%%%%%%%%
method(T-_-_, 
       cs_add, 
        seq(cs_simp,   B-A-Sh, B2-A2-Sh2, Ps), 
       [seq(cs_simp1, B1-A1-Sh, B2-A2-Sh2, [])], 
       true, 
       cs_add) :- 
	(T =..[_,eq]; T =eq),
	(Ps\==[] -> 
	    (ord_union(Ps,B,B1), 
	     ord_union(Ps,A,A1))
	). 

method(T-_-_, 
       cc_merge, 
       seq(cs_simp1,B-A-Sh,B-A1-Sh1,[]), 
       [], 
       true, 
       cc_merge) :- 
	(T =..[_,eq]; T =eq),
	select_equs(A,Equs,A1),
	cc_extend(Equs,Sh,Sh1), Sh\==Sh1, !.

method(T-_-_, 
       cc_canon, 
       seq(cs_simp1, B-A-Sh, B-A1-Sh, []), 
       [], 
       true, 
       cc_canon) :- 
	(T =..[_,eq]; T =eq),
	select(P,A,A0), 
	cc_canon(Sh,P,P1), 
	ord_add_element(A0,P1,A1). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GROUND LEMMA SPECULATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gls(T, P, C, C1, Q, ConstrList,Technique):- returns a new
%                               constraint store C1, given a set
%                               of input literals P, a constraint
%                               store C, the list of conclusions of
%                               constraints rules (ConstrList).  
%                               Finally, T is the parameter theory
%                               (in this file can be aug(eq)), 
%                               and Technique is the name of the
%                               technique of lemma speculation used. 
%
% Notice that gls must return an ground version of ConstrList!

% Ground Lemma Speculation: augment technique
gls(aug(eq)-Ex, P, B-A-Sh, B1-A-Sh, Q, [S=T],augment):-
	crule(aug(eq)-Ex, Q, S=T),
	arrow(aug(eq), P, B-A-Sh, S=T, B1-A-Sh).

%%%%%% Interface to augmentation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% arrow(T, Q, P, C, Constr, C1) :- returns a new constraint store C1,
%                               given a set of input literals P, a
%                               constraint store C, the conclusion of
%                               a constraint rule (Constr).  Finally,
%                               T is the parameter theory (in this
%                               file aug(eq)).
% Notice that arrow must return an ground version of Constr!
% Furthermore arrow must enjoy the properties listed in the paper
% "Termination of Constraint Contextual Rewriting" (pages 10 and 11)
% in order to ensure both the soundness and the termination of CCR. 
arrow(aug(eq), P, B-A-Sh, S=T, B1-A-Sh) :-
	prop_args(S=T, Args),
	max_term(Arg, Args),
	eq_aug_occurs(Arg, P, B-A-Sh), 
	\+ member(S=T, B),
	ord_add_element(B, S=T, B1).

eq_aug_occurs(Term, _, _-_-(_-Us-_)) :- 
	member(_-U, Us), member(Term, U). 
% eq_aug_occurs(Term, P,_)            :- 
% 	subexp_at(P, _, Term). 
% eq_aug_occurs(Term, _, _-A-_)        :- 
% 	subexp_at(A, _, Term). 








