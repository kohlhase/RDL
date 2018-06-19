:- ensure_loaded(library(lists)).
:- ensure_loaded(base).
:- ensure_loaded(ordering).

:- multifile method/6, gls/7.

%%%%%%%%%%%%%%%%%% CONTEXT SIMPLIFICATION AND EXTENSION %%%%%%%%%%%%%%%%%%%% 
method(_-_-_,
       cs_extend,
        seq(cs_extend, C, C1, P),
       [seq(cs_simp,   C, C1, P)],
       true,
       cs_extend).

method(T-Ex-Ord,
       lemma_speculation,
        seq(cs_extend, C, C2, P),
       [seq(rlv,       Q, [], C1),
	seq(cs_simp,  C1, C2, ConstrList)],
        true, 
       Tac) :-
	gls(T-Ex, P, C, C1, Q, ConstrList,Tac),
	ground(Q-ConstrList),
%	ordering(Ex, T, Ord),
	msgr(T-Ex-Ord, ConstrList, Q).
