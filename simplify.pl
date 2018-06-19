:- ensure_loaded(library(lists)).
:- ensure_loaded(library(ordsets)).
:- ensure_loaded(base).

:- multifile method/6.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% CLAUSE SIMPLIFICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This method takes a list and returns an ordered set, since clauses
% are internally represented as ordered sets and externally as lists. 
% The sequent carrying the external representation of clauses is
% labeled "simp", while that carrying the interanl representation is
% labeled "osimp".
method(_,
       osimp,                    % name of the method
       seq(simp,E,E1,[]),        % conclusion: sequent of type simp
       [seq(osimp,OE,E1,[])],    % premises:   sequent of type osimp
       true,                     % no side-conditions
       osimp) :-                 % tactic name
	clause_to_oclause(E,OE).

% This method checks whether the input clause contains the literal
% true and in that case simplifies it to the empty clause.
method(_,
       cl_true,                  % name of the method
       seq(osimp,E,[true],[]),   % conclusion: sequent of type osimp
       [],                       % premises:   none
       true,                     % no side-conditions 
       cl_true) :-               % tactic name
	member(true,E).

% This method checks whether the input clause contains the literal
% false and in that case simplifies it by removing the first
% occurrence of the literal false in the clause.
method(_,
       cl_false,                 % name of the method
       seq(osimp,E,E1,[]),       % conclusion: sequent of type osimp
       [],                       % premises:   none
       true,                     % no side-conditions 
       cl_false) :-              % tactic name
	select_focus_literal(false,E,E1).


% This method tries to simplify the clause by rewriting the focus
% literal by means of constraint contextual rewriting, i.e. by
% contextual rewriting the literal and allowing the context to be
% accessed by the decision procedures.
method(T-_-_,
       cl_simp,                  % name of the method
       seq(osimp,E,E1,[]),       % conclusion: sequent of type osimp
       [seq(cs_extend,C0,C,NE0), % premises:   sequent of type cs_fe and
        seq(ccr,P,P1,C)],        %             sequent of type ccr
       ord_add_element(E0,P1,E1),% FAKE side condition
       cl_simp) :-               % tactic name
	select_focus_literal(P,E,E0),
	cs_init_state(T,C0),     
	negate(E0,NE0).

% This predicate selects a focus literal P from the context E
% and returns in E0 the context E without the literal P
select_focus_literal(P,E,E0):-
	select(P,E,E0).










