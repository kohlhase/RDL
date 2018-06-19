%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% reduce %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reduce(T, seq(SN,E1,E3,P), Tac:Tacs>Tac1) :-
	method(T, _, seq(SN,E1,E2,P), O, Post, Tac),
	reduce_multi(T, O, Tacs),
	Post,
	E1\==E2, !,
	reduce(T,seq(SN,E2,E3,P),Tac1).
reduce(_, seq(_,E,E,_), id).

reduce_multi(_,[],[]).
reduce_multi(T, [S|Ss], [M|Ms]) :-
	reduce(T,S,M), 
	reduce_multi(T,Ss,Ms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% reduce for debugging %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this is for complete debugging
is_an_important_method(M) :- 
	\+ member(M, [cc_canon, cc_merge, push_polys]).

reduce_for_debugging(T, S, CTac) :- 
	format("\n===== Start tracing =====================\n\n", []),
	format("\n\n(Depth, Transistive Closure Steps, Premise Number)\n\n", []),
	reduce_for_debugging0(T, S, CTac, -1, 0, 0),
	format("\n===== End tracing =======================\n\n", []).

reduce_for_debugging0(T, seq(SN,E1,E3,P), Tac:Tacs>Tac1, Depth, Hcl, Pnum) :-
	method(T,Name,seq(SN,E1,E2,P),O,Post,Tac),
	NewDepth is (Depth+1),
        %%%% Just for displaying %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    (is_an_important_method(Name) ->
	(
	format("\n--(~d,~d,~d)-- Method name: ~w ---------\n", 
	       [NewDepth, Hcl, Pnum, Name]),
   	format("\nPremises: ",        []),              write(O), 
   	format("\nConclusion: ~w",    seq(SN,E1,E2,P)),
   	format("\nSide conditions: ", []),              write(Post),
   	format("\n-------------------------------------------------\n\n", [])
    );
	format("~w, ", Name)),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	reduce_multi_for_debugging0(T,O,Tacs, NewDepth, Hcl, 1),
	Post,
	E1\==E2, !,
	% Performing the (horizontal) transitive closure
	NewHcl is (Hcl+1),
	reduce_for_debugging0(T,seq(SN,E2,E3,P),Tac1, NewDepth, NewHcl, Pnum).
reduce_for_debugging0(_,seq(_,E,E,_),id, Depth, Hcl, Pnum) :-
 	NewDepth is (Depth+1),
	format("--(~d,~d,~d)-- Method name: ~w --------\n\n", 
	       [NewDepth, Hcl, Pnum, id]).

reduce_multi_for_debugging0(_,[],[], _, _, _).
reduce_multi_for_debugging0(T,[S|Ss],[M|Ms], Depth, Hcl, Pnum) :-
	reduce_for_debugging0(T,S,M, Depth, Hcl, Pnum), 
	NewPnum is (Pnum+1),
	reduce_multi_for_debugging0(T,Ss,Ms, Depth, Hcl, NewPnum).

% Here it follows a description of the methods.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   CALL        : method (T-Ex,                           Parameter:
%                                                          theory and
%                                                          example name
%                         r,                              method name      
%                         c::e --i--> e',                 input sequent 
%                         cond0,                          applicability 
%                                                         condition
%                         [<(c1::e1 --i1--> e1', cond1),  1st premise and 
%                                                         its applicability 
%                                                         condition
%                           ..., 
%                           (cn::eN --iN--> eN', condN)], last premise and 
%                                                         its applicability 
%                                                         condition
%                         tac)                            tactic
%              
%   ARGUMENTS   : r              = name of the method
%                 c::e --i--> e' = input sequent                   
%                 [<(c1::e1 --i1--> e1'), cond1>, 
%                  ..., 
%                  <(cn::eN --iN--> eN'), condN>] 
%                                = premise and its applicability
% 			           condition, ..., last premise and
% 			           its applicability condition  
%                 tac            = tactic, a program s.t., given any
% 	                           closed derivation of the premises
%                                  c1::e1 --i1--> e1', c2::e2 --i2--> e2',
%                                  ..., cN::eN --iN--> eN' returns a
%                                  closed derivation of the input 
%                                  sequent c::e --i--> e' (provided it 
%                                  terminates successfully) and cond0,
%                                  cond1, ..., condN is a finite
%  			           sequence of meta-logical predicates 
%
%   ASSUMPTIONS : We assume that such prediactes are part of a
%                 meta-logic which faithfully encodes the properties
%                 expressed by the conditions of the inference rules.
%                 We also assume that our meta-logic is expressed as a
%                 logic program enjoing some functional features such
%                 as interpreted function symbols, not freely
%                 generated data structures.  Finally, we assume that
%                 each predicate p (and each interpreted function
%                 symbol) of the meta-logic comes equipped with a
%                 definition and a mode s.t. the evaluation of any
%                 query of the form p(t1, ..., tN) with IVARS(p(t1,
%                 ..., tN)) = emptyset terminates.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

