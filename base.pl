proposition(T,P) :- pred_sym(T,P).
proposition(T,not(P)) :- proposition(T,P).

negate([],[]).
negate([P|Ps],NPs1) :-
	complementary(P,NP),
	negate(Ps,NPs),
	ord_add_element(NPs,NP,NPs1).

complementary(not(P),P) :- !.
complementary(P,not(P)).

unsat(A) :- 
	member(not(X=X),A), !.
unsat(A) :-
	member(X,A),
	complementary(X,XC),
	member(XC,A), !.

select_equs([],[],[]).
select_equs([X=Y|Ls],[X=Y|Es],Rs) :- !,
	select_equs(Ls,Es,Rs).
select_equs([L|Ls],Es,[L|Rs]) :-
	select_equs(Ls,Es,Rs).

clause_to_oclause([],[]).
clause_to_oclause([L|Ls],OC2) :-
	clause_to_oclause(Ls,OC1),
	add_lit_to_oclause(OC1,L,OC2).

add_lit_to_oclause([],L,[L]).
add_lit_to_oclause([L|OC],L1,[L1,L|OC]) :-
	lit_lte(L1,L), !.
add_lit_to_oclause([L|OC],L1,[L|OC1]) :-
	add_lit_to_oclause(OC,L1,OC1).

lit_lte(not(X),not(Y)) :- !, X @=< Y.
lit_lte(_,not(_)) :- !.
lit_lte(not(_),_) :- !, fail.
lit_lte(X,Y) :- X @=< Y.

