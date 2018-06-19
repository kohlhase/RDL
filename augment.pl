:- ensure_loaded(library(terms)).

prop_args(not(P),Args) :- !, prop_args(P,Args).
prop_args(P,Args)      :- P=..[_|Args].

max_term(T,Ts) :-
	select(T,Ts,Ts0),
	\+ var(T),
	greatest_term(T,Ts0).

greatest_term(_,[]).
greatest_term(T0,[T|Ts]) :-
	T0 @>= T, 
	term_variables(T0,T0Vs),
	term_variables(T,TVs),
	varlist_subseteq(TVs,T0Vs),
	greatest_term(T0,Ts).

varlist_subseteq([],_).
varlist_subseteq([V|Vs],Ws) :-
	member_var(V,Ws),
	varlist_subseteq(Vs,Ws).

member_var(V,[W|_]) :- V==W, !.
member_var(V,[_|Ws]) :- member_var(V,Ws).




