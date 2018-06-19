:- ensure_loaded(library(lists)).
:- ensure_loaded(library(ordsets)).

:- ensure_loaded(base).
:- ensure_loaded(reduce).
:- ensure_loaded(simplify).
:- ensure_loaded(cs_extend).
:- ensure_loaded(ccr).
:- ensure_loaded(augment).
:- ensure_loaded(non_linear).

:- use_module(shostak).
:- use_module(fourier).

:- multifile method/6, cs_init_state/2, cs_unsat/2, gls/7, arrow/5.

%%%%%% Interface to the constraint solver %%%%%%%%%%%%%%%%%%%%%%%%%
cs_init_state(T,[]-[]-Sh0-F0) :-
	(T =..[_,eq_la]; T =eq_la),
	cc_init(Sh0),
	arith_init(F0).
cs_unsat(T,_-A-_-_) :- 
	(T =..[_,eq_la]; T =eq_la),
	unsat(A).
cs_unsat(T,_-_-_-F) :- 
	(T =..[_,eq_la]; T =eq_la),
	arith_unsat(F).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% Interface method to ccr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
method(T-Ex-Ord,
       normal,
        seq(cs_normal, E, E1, _-_-Sh-_),
       [],
       true,
       cs_canon) :-
	(T =..[_,eq_la]; T =eq_la),
	cc_canon(Sh, E, E1),
	ord(T-Ex-Ord,E,E1,gr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Constraint Simplification %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  seq(cs_simp(B-A-Sh-F,B1-A1-Sh1-F1,Es)
%%%    -  B: is the set of fact currently being added by augment
%%%    -  A: is a set of literals
%%%    -  S: state of the separate procedure (i.e. a counter)
%%%    - Sh: state of the Shostak's congruence closure algorithm
%%%    -  F: state of the Fourier's decision procedure
%%%    - Es: set of facts curretly being canonised. Used by augment
%%%          to look into the expression currently being canonised.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Methods for constraint simplification %%%%%%%%%%%%%%%%%%%%%%
method(T-_-_,
       cs_add,
       seq(cs_simp,      B-A-Sh-F, B2-A2-Sh2-F2, Ps),
       [seq(cs_simp1,  B1-A1-Sh-F, B2-A2-Sh2-F2, [])],
       true,
       cs_add) :-
	(T =..[_,eq_la]; T =eq_la),
 	list_to_ord_set(Ps, OPs),
 	ord_union(OPs,B,B1), 
 	ord_union(OPs,A,A1).

method(T-_-_,
       cc_merge,
       seq(cs_simp1, B-A-Sh-F, B-A-Sh1-F, []),
       [],
       true,
       cc_merge) :-
	(T =..[_,eq_la]; T =eq_la),
	( setof(L=R, arith_entailed_equ(L=R, F), Equs2) ; Equs2=[] ),
        append(A, Equs2, A2),
	cc_extend(A2, Sh, Sh1).

method(T-_-_,
       cc_canon,
       seq(cs_simp1, B-A-Sh-F, B-A1-Sh-F, []),
       [],
       true,
       cc_canon) :-
	(T =..[_,eq_la]; T =eq_la),
	member(P,A),
	cc_canon(Sh, P, P1),
	ord_add_element(A, P1, A1).

method(T-_-_,
       push_poly,
       seq(cs_simp1, B-A-(Sh0-EntEqs)-Cs, B-A-(Sh0-EntEqs)-Cs1, []),
       [],
       true,
       push_poly) :-
	(T =..[_,eq_la]; T =eq_la),
	arith_extend(A,Cs,Cs0),
	arith_extend(EntEqs,Cs0,Cs1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GROUND LEMMA SPECULATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gls(T, P, C, C1, Q, ConstrList,Technique):- returns a new
%                               constraint store C1, given a set
%                               of input literals P, a constraint
%                               store C, the list of conclusions of
%                               constraints rules (ConstrList).  
%                               Finally, T is the parameter theory
%                               (in this file can be aug(eq_la)), 
%                               and Technique is the name of the
%                               technique of lemma speculation used. 
%
% Notice that gls must return an ground version of ConstrList!

% Ground Lemma Speculation: Combination of augment and 
%                           affinize techniques
gls(aug_aff(eq_la)-Ex, _, B-A-Sh-F, B1-A-Sh-F, Q, ConstrList,augment_affinize) :-
	(
	    gls(aff(eq_la)-Ex, _, B-A-Sh-F, B1-A-Sh-F, Q, ConstrList,_)
	; 
	    gls(aug(eq_la)-Ex, _, B-A-Sh-F, B1-A-Sh-F, Q, ConstrList,_)
	).

% Ground Lemma Speculation: affinize technique
gls(aff(eq_la)-_, _, B-A-Sh-F, B1-A-Sh-F, Q, ConstrList,affinize) :-
	F=_-_-Pot,
	member(K-Pos-Neg, Pot),
	K=..[*|_],
	append(Pos, Neg, PosNeg),
	member(Ineq, PosNeg),           % choose an inequality in the store
	\+ member(Ineq, B),             % check whether it has not
	                                % been considered  
	ord_add_element(B, Ineq, B1),   % make sure that the current
	                                % inequality will not be
	                                % considered again
        strip_dependency_list(Ineq, SIneq),
	adapt_inequality(SIneq, ASIneq),
	                                % forget the dependency list
	                                % of the inequality
	factorize(ASIneq, Res),         % find out if there it is
	                                % possible to apply
	                                % affinization and returns the
	                                % two factors
        affinize(Res, Q-ConstrList).    % takes the result of the
	                                % factorization step and then
	                                % returns a logically
	                                % equivalent (over integers)
	                                % disjunction of affinized
	                                % inequalities toghether with
	                                % the associated hypotheses
	                                % under which each disjunct
	                                % holds 

% %
% % Ground Lemma Speculation: affinize technique
% gls(aff(eq_la)-_, _, BF-A-Sh-F, BF1-A-Sh-F, [], [Constr],affinize) :-
% 	F=_-_-Pot,
% 	Pot=[K-Pos-Neg|_],
% 	K=..[*|_],
% 	append(Pos, Neg, PosNeg),
% 	member(Ineq, PosNeg),           % choose an inequality in the store
% 	\+ member(Ineq, BF),             % check whether it has not
% 	                                % been considered  
% 	ord_add_element(BF, Ineq, BF1),   % make sure that the current
% 	                                % inequality will not be
% 	                                % considered again
%         strip_dependency_list(Ineq, SIneq),
%         drop_pos_power(SIneq, Constr).


% Ground Lemma Speculation: augment technique
gls(aug(eq_la)-Ex, P, B-A-Sh-Cs, B1-A-Sh-Cs, Q, [C],augment):-
	crule(aug(eq_la)-Ex, Q, C),
	arrow(aug(eq_la), P, B-A-Sh-Cs, C, B1-A-Sh-Cs).

%%%%%% Interface to augmentation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% arrow(T, Q, P, C, Constr, C1) :- returns a new constraint store C1,
%                               given a set of input literals P, a
%                               constraint store C, the conclusion of
%                               a constraint rule (Constr).  Finally,
%                               T is the parameter theory (in this
%                               file aug(eq_la)).
% Notice that arrow must return an ground version of Constr!
% Furthermore arrow must enjoy the properties listed in the paper
% "Termination of Constraint Contextual Rewriting" (pages 10 and 11)
% in order to ensure both the soundness and the termination of CCR. 
arrow(aug(eq_la), P, B-A-Sh-Cs, C, B1-A-Sh-Cs) :-
	prop_args(C, Args),
	max_term(Arg, Args),
	eq_la_aug_occurs(Arg, P, B-A-Sh-Cs), 
	cc_canon(Sh, C, C1),
	\+ member(C1, B),
	ord_add_element(B, C1, B1).

eq_la_aug_occurs(T,_,_-_-_-(_-_-Cs)) :- member(T-_-_,Cs).




