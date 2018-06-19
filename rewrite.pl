:- ensure_loaded(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%  Low level machinery for rewriting   %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% T occurs at position P in S %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subexp_at(S,P,T) :-
	subexp_at(S,[],P,T).
subexp_at(S,P,P,S).
subexp_at(S,P,P1,T) :-
	S=..[_|Args],
	nth(N,Args,Arg),
	subexp_at(Arg,[N|P],P1,T).

replace_at(S,P,R,T) :-
	reverse(P,RP),
	replace_at0(S,RP,R,T).
replace_at0(_,[],R,R).
replace_at0(S,[N|P],R,T) :-
	S=..[F|Args],
	select_nth(N,Args,Pre,Arg,Post),
	replace_at0(Arg,P,R,Arg1),
	append(Pre,[Arg1|Post],Args1),
	T=..[F|Args1].

select_nth(1,[Arg|Post],[],Arg,Post) :- 
	!.
select_nth(N,[E|Es],[E|Pre],Arg,Post) :-
	N1 is N-1,
	select_nth(N1,Es,Pre,Arg,Post).
