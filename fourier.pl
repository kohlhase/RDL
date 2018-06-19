:- module(new_fourier, [ arith_init/1,
		   arith_extend/3,
		   arith_unsat/1,
		   arith_entailed_equ/2 ]).

:- ensure_loaded(library(ordsets)).
:- ensure_loaded(library(lists)).
:- ensure_loaded(normalise_lineq).

:- op(701,xfx,<--).

% A multiplicant (MULT) is term different from an integer and
% whose top-most function symbol is different from +, -, *.
% A normal form linear polynomial (NFLP) p is a list of the form
% [r_1*m_1,...,r_n*m_n] where r_i is a rational different from rat(0,D) and
% m_i is a MULT for i=1,...,n and m_i @< m_(i+1) for i=1,...,(n-1),
% with n>=0. 
%
% A linear fact is an
% expression of the form NFLP=<N<--DL where NFLP is a NFLP, N an integer,
% and DL is the Dependency List of the constraint (i.e., a list of linear
% constraint used to derive the constraint NFLP=<N).
%
% A Constraint Store is a triple UNSAT-ENTEQ-Cs where:
% - UNSAT is a list of linear facts of the form []=<N<--DL with N<0
% - ENTEQ is a list of linear facts of the form []=<0<--DL
% - Cs is an ordered set of linear facts.

arith_init([]-[]-[]).
arith_normal(_,E,E).
arith_unsat([_|_]-_-_).
arith_entailed_equ(L=R,_-ENTEQ-_) :-
	member([]=<0<--DL,ENTEQ),
	member(LF,DL),
	extract_equ(LF,L=R).

extract_equ(P=<N,M=LT) :-
	select(X*E,P,P1),
	AX is abs(X),
	S is X//AX,
	mults2lt(P1,N,S,LT),
	( AX=1 ->
	    M=E ;
	    M=AX*E ).

mults2lt([],N,X,LT) :- !,
        LT is X*N. 

mults2lt(P,0,X,LT) :- !,
	mults2lt(P,X,LT).
mults2lt(P,N,X,LT+N1) :-
	mults2lt(P,X,LT),
	N1 is N*X.
mults2lt([Y*E],X,LT) :-
	Z is -1*X*Y,
	mult2lt(Z,E,LT).
mults2lt([Y*E|P],X,LT1+LT2) :-
	Z is -1*X*Y,
	mult2lt(Z,E,LT1),
	mults2lt(P,X,LT2).
mult2lt(1,E,E) :- !.
mult2lt(-1,E,-E) :- !.
mult2lt(Z,E,Z*E).

% We first add the (trivial) dependency list to the input linear
% inequalities (Ls), then we invoke the main procedure, i.e. arith_extend1.
arith_extend(Cs0,CS,CS1) :-
	normalise_lineq_multi(Cs0,Ls),
	lineqs_add_dl(Ls,LsDL),
	push_polys(LsDL,CS,CS1).

lineqs_add_dl([],[]).
lineqs_add_dl([L|Ls],[L<--[L]|LsDL]) :-	lineqs_add_dl(Ls,LsDL).

push_polys([],CS,CS).
push_polys([[]=<0<--DL|Es],CS,UNSAT-ENTEQ1-Cs1) :- !,
	push_polys(Es,CS,UNSAT-ENTEQ-Cs1),
	ord_add_element(ENTEQ,[]=<0<--DL,ENTEQ1).
push_polys([E|_],_-ENTEQ-OTHERS,[E]-ENTEQ-OTHERS) :-
	unfeasible_fact(E), !.
push_polys([E|Es],UNSAT-ENTEQ-OTHERS,CS) :-
	subsumed_fact(E,OTHERS), !,
	push_polys(Es,UNSAT-ENTEQ-OTHERS,CS).
push_polys([[K*A|Ps]=<N<--DL|Es],UNSAT-ENTEQ-Cs,CS1) :-
	select(A-Pos-Neg,Cs,Cs1), !,
	( K>0 ->
	    Ls=Neg, Cs2=[A-[[K*A|Ps]=<N<--DL|Pos]-Neg|Cs1] ;
	    Ls=Pos, Cs2=[A-Pos-[[K*A|Ps]=<N<--DL|Neg]|Cs1] ),
	push_poly_1step([K*A|Ps]=<N<--DL,Ls,Es1),
	append(Es1,Es,Es2),  
	push_polys(Es2,UNSAT-ENTEQ-Cs2,CS1).
push_polys([[K*A|Ps]=<N<--DL|Es],UNSAT-ENTEQ-Cs,CS1) :-
	K>0, !,
	push_polys(Es,UNSAT-ENTEQ-[A-[[K*A|Ps]=<N<--DL]-[]|Cs],CS1).
push_polys([[K*A|Ps]=<N<--DL|Es],UNSAT-ENTEQ-Cs,CS1) :-
	push_polys(Es,UNSAT-ENTEQ-[A-[]-[[K*A|Ps]=<N<--DL]|Cs],CS1).

push_poly_1step(_,[],[]).
push_poly_1step(E,[C|Cs],Cs2) :-
	xmpy_add_poly(E,C,C1), !,
	push_poly_1step(E,Cs,Cs1),
	ord_add_element(Cs1,C1,Cs2).
push_poly_1step(E,[_|Cs],Cs1) :-
	push_poly_1step(E,Cs,Cs1).

unfeasible_fact([]=<N<--_) :- N<0.
trivial_fact([]=<N<--_) :- N>=0.

subsumed_fact([]=<N<--_,_) :- N>0, !.
subsumed_fact([K*E|P]=<N<--_,Cs) :-
	member(E-Pos-Neg,Cs), !,
	( K>0 -> Ls=Pos ; Ls=Neg ),
	member([K*E|P]=<N1<--_,Ls), !,
	N1=<N.

xmpy_add_poly([K1*E|NFLP1]=<N1<--DL1,[K2*E|NFLP2]=<N2<--DL2,NFLP=<N<--DL) :-
	-1 is sign(K1*K2),
 	complementary(K1,K2,H1,H2), !,
	N0 is N1*H2+N2*H1,
	GCD is max(1,abs(N0)),
	xmpy_and_add_polys(GCD,GCD1,H1,NFLP1,H2,NFLP2,NFLP0),
	divide_poly(GCD1,NFLP0=<N0,NFLP=<N),
	append(DL1,DL2,DL).

xmpy_and_add_polys(GCD,GCD,_,[],_,[],[]).
xmpy_and_add_polys(GCD,GCD2,X,[],Y,[Z*E|P],R) :- !,
	W is Z*X, 
	GCD1 is gcd(GCD,W),
	xmpy_and_add_polys(GCD1,GCD2,X,[],Y,P,R1),
	( W=0 -> R=R1 ; R=[W*E|R1]).
xmpy_and_add_polys(GCD,GCD2,X,[Z*E|P],Y,[],R) :- !,
	W is Z*Y,
	GCD1 is gcd(GCD,W),
	xmpy_and_add_polys(GCD1,GCD2,X,P,Y,[],R1),
	(W=0 -> R=R1 ; R=[W*E|R1]).
xmpy_and_add_polys(GCD,GCD2,X,[Z*E|P],Y,[W*E|Q],R) :- !,
	V is Z*Y+W*X,
	GCD1 is gcd(GCD,V),
	xmpy_and_add_polys(GCD1,GCD2,X,P,Y,Q,R1),
	(V=0 -> R=R1 ; R=[V*E|R1]).
xmpy_and_add_polys(GCD,GCD2,X,[Z*E|P],Y,[W*F|Q],R) :-
	term_gt(E,F), !,
	V is Z*Y,
	GCD1 is gcd(GCD,V),
	xmpy_and_add_polys(GCD1,GCD2,X,P,Y,[W*F|Q],R1),
	(V=0 -> R=R1 ; R=[V*E|R1]).
xmpy_and_add_polys(GCD,GCD2,X,[Z*E|P],Y,[W*F|Q],R) :-
	V is W*X,
	GCD1 is gcd(GCD,W),
	xmpy_and_add_polys(GCD1,GCD2,X,[Z*E|P],Y,Q,R1),
	(V=0 -> R=R1 ; R=[V*F|R1]).

complementary(X,Y,X1,Y) :-
	X < 0, !,
	Y > 0,
	X1 is -X.
complementary(X,Y,X,Y1) :-
	Y < 0,
	Y1 is -Y.







