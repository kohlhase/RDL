:- module(normalise_lineq,
	  [ normalise_lineq/2,
	    normalise_lineq_multi/2,
	    divide_poly/3,
	    term_gt/2 ]).

:- ensure_loaded(library(lists)).

normalise_lineq_multi([],[]).
normalise_lineq_multi([E|Es],LLs) :-
	normalise_lineq(E,L),
	normalise_lineq_multi(Es,Ls),
	append(L,Ls,LLs).

normalise_lineq(L=<M,[LI]) :- !,
	normalise_laexp(L-M,P),
	laexp2lineq(P,LI).
normalise_lineq(L<M,[LI]) :- !,
	normalise_laexp(L-M+1,P),
	laexp2lineq(P,LI).
normalise_lineq(L>M,[LI]) :- !,
	normalise_laexp(M-L+1,P),
	laexp2lineq(P,LI).
normalise_lineq(L>=M,[LI]) :- !,
	normalise_laexp(M-L,P),
	laexp2lineq(P,LI).
normalise_lineq(L=L,[]) :- !.
normalise_lineq(L=M,[LI1,LI2]) :- !,
	normalise_laexp(L-M,P1),
	laexp2lineq(P1,LI1),
	normalise_laexp(M-L,P2),
	laexp2lineq(P2,LI2).
normalise_lineq(not(L=<M),C) :- !,
	normalise_lineq(L>M,C).
normalise_lineq(not(L<M),C) :- !,
	normalise_lineq(L>=M,C).
normalise_lineq(not(L>=M),C) :- !,
	normalise_lineq(L<M,C).
normalise_lineq(not(L>M),C) :- !,
	normalise_lineq(L=<M,C).
normalise_lineq(not(L=M),[[]=< -1]) :-
	normalise_laexp(L-M,[0]), !.
normalise_lineq(_,[]).

laexp2lineq(NFP,N1=<P1) :-
	laexp2lineq1(NFP,P=<N),
	GCD is max(1,abs(N)),
	compute_gcd(GCD,GCD1,P),
	divide_poly(GCD1,P=<N,N1=<P1).

laexp2lineq1([],[]=<0).
laexp2lineq1([N|P],P=<M) :-
	number(N), !,
	M is -N.
laexp2lineq1(P,P=<0).

compute_gcd(GCD,GCD,[]).
compute_gcd(GCD,GCD2,[N*_|P]) :-
	GCD1 is gcd(GCD,N),
	compute_gcd(GCD1,GCD2,P).

normalise_laexp(E,P) :-
	normalise_laexp(E,[0],P,+).
normalise_laexp(X,[Y|P],[Z|P],Pol) :-
	number(X), !,
	pol_add(Z,Y,X,Pol).
normalise_laexp(-E,P,P1,Pol) :- !,
	flip_pol(Pol,NewPol),
	normalise_laexp(E,P,P1,NewPol).
normalise_laexp(E1+E2,P,P2,Pol) :- !,
	normalise_laexp(E1,P,P1,Pol),
	normalise_laexp(E2,P1,P2,Pol).
normalise_laexp(E1-E2,P,P2,Pol) :- !,
	normalise_laexp(E1,P,P1,Pol),
	flip_pol(Pol,NewPol),
	normalise_laexp(E2,P1,P2,NewPol).
normalise_laexp(X*E,[C|P],[C|P2],Pol) :-
	number(X), !,
	( select(Y*E,P,P1) ->
	    pol_add(Z,Y,X,Pol),
	    mult_insert(Z*E,P1,P2) ;
	    pol_add(Z,0,X,Pol),
	    mult_insert(Z*E,P,P2) ).
normalise_laexp(E*X,[C|P],[C|P2],Pol) :-
	number(X), !,
	( select(Y*E,P,P1) ->
	    pol_add(Z,Y,X,Pol),
	    mult_insert(Z*E,P1,P2) ;
	    pol_add(Z,0,X,Pol),
	    mult_insert(Z*E,P,P2) ).
normalise_laexp(E,[C|P],[C|P2],Pol) :-
	( select(Y*E,P,P1) ->
	    pol_add(Z,Y,1,Pol),
	    mult_insert(Z*E,P1,P2) ;
	    pol_add(Z,0,1,Pol),
	    mult_insert(Z*E,P,P2) ).

flip_pol(+,-).
flip_pol(-,+).
pol_add(Z,Y,X,+) :- Z is Y+X.
pol_add(Z,Y,X,-) :- Z is Y-X.

divide_poly(GCD,P=<N,P1=<N1) :-
	N1 is N//GCD,
	divide_poly1(GCD,P,P1).
divide_poly1(_,[],[]).
divide_poly1(GCD,[N*E|Ps],[N1*E|Ps1]) :-
	N1 is N//GCD,
	divide_poly1(GCD,Ps,Ps1).

mult_insert(0*_,P,P) :- !.
mult_insert(X*E,[X1*E1|Rest],[X*E,X1*E1|Rest]) :-
	term_gt(E,E1), !.
mult_insert(T,[T1|Ts],[T1|Ts1]) :-
	mult_insert(T,Ts,Ts1).
mult_insert(T,[],[T]).

mult_member(Y*E,[Y*E|_]) :- !.
mult_member(Y*E,[_*E1|NFLP]) :-
	term_gt(E,E1),
	mult_member(Y*E,NFLP).


%%% ordering over monomials %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
term_gt(S1*S2,T1*T2) :-
	term_gt0(S1*S2,T1*T2).

term_gt(S,T1*T2) :-
	\+ flatten_wd(T1*T2, [_]),
	S=..[F|_],
	\+ member(*,[F]).

term_gt(T1*T2, S) :-
	\+ flatten_wd(T1*T2, [_]),
	S=..[F|_],
	!, 
	(member(*,[F]) ->
	    term_gt0(T1*T2, S) ;
	    true).
% AA: USED TO BE.  This makes bm102 work but nl-bjorner fail
% 	(member(*,[F]) -> term_gt0(T1*T2, S) ).

term_gt(S,T) :-
	term_gt0(S,T).

term_gt0(S,T) :-
	term_size(S,NS),
	term_size(T,NT),
	( NS>NT -> true ;
	    ( NS=NT -> S@>T ) ).

term_size([],0) :- !.
term_size([T|Ts],N1) :- !,
	term_size(T,N),
	term_size(Ts,Ns),
	N1 is N+Ns.
term_size(T,N) :-
	T=..[_|Args],
	term_size(Args,Ns),
	N is Ns+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_not_a_times_application(_*_) :- 
	!, fail.
is_not_a_times_application(_).

flatten_wd(V, [V]) :- 
	is_not_a_times_application(V).
flatten_wd(A*B, List) :- 
	is_not_a_times_application(A),
	is_not_a_times_application(B), 
	( A\==B -> List=[A,B] ; List=[A]), !.

flatten_wd(A*B, JJ) :- 
	flatten_wd(A, A1),
	flatten_wd(B, B1),
	append(A1, B1, AtimesBlist),
	remove_duplicates(AtimesBlist, J),
	filter_numbers(J, JJ).

filter_numbers([],[]).
filter_numbers([H|T],T1) :-
	number(H), !,
	filter_numbers(T,T1).
filter_numbers([H|T],[H|T1]) :-
	filter_numbers(T, T1).

