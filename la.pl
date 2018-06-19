:- ensure_loaded(library(lists)).
:- ensure_loaded(library(ordsets)).
:- ensure_loaded(library(terms)).

:- ensure_loaded(base).
:- ensure_loaded(reduce).
:- ensure_loaded(simplify).
:- ensure_loaded(cs_extend).
:- ensure_loaded(ccr).
:- ensure_loaded(augment).
:- ensure_loaded(non_linear).

:- use_module(normalise_lineq).
:- use_module(fourier).

:- multifile method/6, cs_init_state/2, cs_unsat/2, gls/7, arrow/5.

%%%%%% Interface to the constraint solver %%%%%%%%%%%%%%%%%%%%%%%%%
cs_init_state(T,[]-[]-Cs) :- 
	(T =..[_,la]; T =la),
	arith_init(Cs).
cs_unsat(T, _-A-_)        :- 
	(T =..[_,la]; T =la),
	unsat(A), !.
cs_unsat(T,_-_-Cs)        :- 
	(T =..[_,la]; T =la),
	arith_unsat(Cs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% Interface method to ccr %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
method(T-Ex-Ord, 
       normal, 
        seq(cs_normal, E, E1, _-_-Cs), 
       [], 
       true, 
       normal) :- 
	(T =..[_,la]; T =la),	
	arith_entailed_equ(L=R,Cs),
	ord(T-Ex-Ord,L,R,O),
	( O==gr -> L1=L, R1=R ;
	    O==nge -> L1=R, R1=L ),
	subexp_at(E,  P, L1),
	replace_at(E, P, R1, E1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Constraint Simplification %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   seq(cs_simp, B-A-F, B1-A1-F1, Ps)
%%%    -  B is the set of fact currently being added by augment
%%%    -  A is a set of literals
%%%    -  F state of the Fourier's LA procedure.
%%%    - Ps input literals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Methods for constraint simplification %%%%%%%%%%%%%%%%%%%%%%
method(T-_-_,
       cs_add,
       seq(cs_simp,     B-A-F, B2-A2-F2, Ps),
       [seq(cs_simp1, B1-A1-F, B2-A2-F2, [])],
       true,
       cs_add) :- 
	(T =..[_,la]; T =la),
	list_to_ord_set(Ps, OPs), 
	ord_union(OPs,B,B1), 
	ord_union(OPs,A,A1).

method(T-_-_,
       push_poly,
       seq(cs_simp1, B-A-Cs, B-A-Cs1, []),
       [],
       true,
       push_poly) :-
	(T =..[_,la]; T =la),
	arith_extend(A,Cs,Cs1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GROUND LEMMA SPECULATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gls(T, P, C, C1, Q, ConstrList,Technique):- returns a new
%                               constraint store C1, given a set
%                               of input literals P, a constraint
%                               store C, the list of conclusions of
%                               constraints rules (ConstrList).  
%                               Finally, T is the parameter theory
%                               (in this file can be either 
%                               aug(la), aff(la) or aug_aff(la)) 
%                               and Technique is the name of the
%                               technique of lemma speculation 
%                               used. 
% Notice that gls must return an ground version of ConstrList!

% Ground Lemma Speculation: Combination of augment and 
%                           affinize techniques
gls(aug_aff(la)-Ex, _, B-A-F, B1-A-F, Q, ConstrList,augment_affinize) :-
	gls(aff(la)-Ex, _, B-A-F, B1-A-F, Q, ConstrList,_).
gls(aug_aff(la)-Ex, _, B-A-F, B1-A-F, Q, ConstrList,augment_affinize) :-
	gls(aug(la)-Ex, _, B-A-F, B1-A-F, Q, ConstrList,_).

% Ground Lemma Speculation: affinize technique
gls(aff(la)-_, _, B-A-F, B1-A-F, Q, ConstrList,affinize) :-
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

% Ground Lemma Speculation: affinize (drop positive powers) technique
 gls(aff(la)-_, _, B-A-F, B1-A-F, [], [Constr],affinize) :-
 	F=_-_-Pot,
 	Pot=[K-Pos-Neg|_],
 	K=..[*|_],
 	append(Pos, Neg, PosNeg),
 	member(Ineq, PosNeg),           % choose an inequality in the store
 	\+ member(Ineq, B),             % check whether it has not
 	                                % been considered  
 	ord_add_element(B, Ineq, B1),   % make sure that the current
 	                                % inequality will not be
 	                                % considered again
	strip_dependency_list(Ineq, SIneq),
	drop_pos_power(SIneq, Constr).

% Ground Lemma Speculation: augment technique
gls(aug(la)-Ex, P, B-A-(UNSAT-ENTEQ-Cs), B1-A-(UNSAT-ENTEQ-Cs), Q, [C],augment):-
	crule(aug(la)-Ex, Q, C),
	arrow(aug(la), P, B-A-(UNSAT-ENTEQ-Cs), C, B1-A-(UNSAT-ENTEQ-Cs)).

%%%%%% Interface to augmentation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% arrow(T, Q, P, C, Constr, C1) :- returns a new constraint store C1,
%                               given a set of input literals P, a
%                               constraint store C, the conclusion of
%                               a constraint rule (Constr).  Finally,
%                               T is the parameter theory (in this
%                               file aug(la)).
% Notice that arrow instantiates C to a ground version!
% Furthermore arrow must enjoy the properties listed in the paper
% "Termination of Constraint Contextual Rewriting" (pages 10 and 11)
% in order to ensure both the soundness and the termination of CCR. 

% CUT PROBLEM !!
% ATTENTION: WITH THE CUT IN THE POSITION AS BELOW
%            WE DON'T UNIFY ALL POSSIBLE UNIFIER TERMS BUT
%            ONLY THE FIRST ONE.  
arrow(aug(la), _, B-A-(UNSAT-ENTEQ-Cs), C, B1-A-(UNSAT-ENTEQ-Cs)) :-
	prop_args(C, Args),
	max_term(Arg, Args), 
	!, % NEW VERSION
	member(Arg-Pos-Neg,Cs), 
%	!, % OLD VERSION
	normalise_lineq(C,LS),
	member([K*Arg|_]=<_,LS),
	( K>0 -> Neg=[_|_] ; Pos=[_|_] ),
	\+ member(C, B),
	ord_add_element(B, C, B1).










