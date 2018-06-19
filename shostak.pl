:- module(cc, [ cc_init/1,
		cc_extend/3,
		cc_canon/3 ]).
:- ensure_loaded(library(lists)).
:- ensure_loaded(library(ordsets)).


cc_init([]-[]-[]-[]).
cc_canon(Sh-_,T,T1) :- canon_star(T,Sh,T1).
	  
cc_extend([],ECs-Us-Ss-EntEqs,ECs-Us-Ss-EntEqs).
cc_extend([A=B|Es],ECs-Us-Ss-IntEqs,ECs4-Us4-Ss4-OutEntEqs) :- !,
	canon(A,ECs-Us-Ss,CanA,ECs1-Us1-Ss1),
	canon(B,ECs1-Us1-Ss1,CanB,ECs2-Us2-Ss2),
	merge(CanA,CanB,ECs2-Us2-Ss2-IntEqs,ECs3-Us3-Ss3-TempEntEqs),
	cc_extend(Es,ECs3-Us3-Ss3-TempEntEqs,ECs4-Us4-Ss4-OutEntEqs).
cc_extend([_|Es],ECs-Us-Ss-InEntEqs,ECs1-Us1-Ss1-OutEntEqs) :-
	cc_extend(Es,ECs-Us-Ss-InEntEqs,ECs1-Us1-Ss1-OutEntEqs).

merge(A,A,ECs-Us-Ss-EntEqs,ECs-Us-Ss-EntEqs) :- !.
merge(A,B,ECs-Us-Ss-InEntEqs,ECs2-Us2-Ss1-OutEntEqs) :-
	ec_union(A,B,ECs-Us-InEntEqs,ECs1-Us1-TempEntEqs),
	( (find(A,ECs,RA), find(A,ECs1,RA)) ->
	    use(A,Us1,UA),
	    merge_up(B,A,UA,ECs1-Us1-Ss-TempEntEqs,ECs2-Us2-Ss1-OutEntEqs) ;
	    use(B,Us1,UB),
	    merge_up(A,B,UB,ECs1-Us1-Ss-TempEntEqs,ECs2-Us2-Ss1-OutEntEqs) ).

merge_up(_,_,[],ECs-Us-Ss-EntEqs,ECs-Us-Ss-EntEqs).
merge_up(A,B,[T|Ts],ECs-Us-Ss-InEntEqs,ECs2-Us2-Ss3-OutIntEqs) :-
	member(T-S_T,Ss), !,
	S_T=..[F|Args],
	myreplace_multi(A,B,Args,Args1),
	S_T1=..[F|Args1],
	ordalist_set(Ss,T-S_T1,Ss1),
	use(B,Us,UB),
	merge_up1(UB,T,S_T1,ECs-Us-Ss1-InEntEqs,ECs1-Us1-Ss2-TempEntEqs),
	use_select(B-UB1,Us1,Us10),
	ord_add_element(UB1,T,NewUB1),
	ord_add_element(Us10,B-NewUB1,NewUs10),
	merge_up(A,B,Ts,ECs1-NewUs10-Ss2-TempEntEqs,ECs2-Us2-Ss3-OutIntEqs).

merge_up1([],_,_,ECs-Us-Ss-EntEqs,ECs-Us-Ss-EntEqs).
merge_up1([V|Vs],T,S_T,ECs-Us-Ss-InEntEqs,ECs2-Us2-Ss2-OutIntEqs) :-
	member(V-S_T,Ss), !,
	find(T,ECs,T1),
	find(V,ECs,V1),
	merge(T1,V1,ECs-Us-Ss-InEntEqs,ECs1-Us1-Ss1-TempEntEqs),
	merge_up1(Vs,T,S_T,ECs1-Us1-Ss1-TempEntEqs,ECs2-Us2-Ss2-OutIntEqs).
merge_up1([_|Vs],T,S_T,ECs-Us-Ss-InEntEqs,ECs1-Us1-Ss1-OutEntEqs) :-
	merge_up1(Vs,T,S_T,ECs-Us-Ss-InEntEqs,ECs1-Us1-Ss1-OutEntEqs).

use(T,Us,U) :- member(T-U,Us), !.
use(_,_,[]).

use_select(T-U,Us,Us0) :- select(T-U,Us,Us0), !.
use_select(_-[],Us,Us).

canon(T,ECs-Us-Ss,T1,ECs1-Us1-Ss1) :-
	canon(T,ECs-Us-Ss,T1,ECs1-Us1-Ss1,yes).
canon_star(T,ECs-Us-Ss,T1) :-
	canon(T,ECs-Us-Ss,T1,ECs-Us-Ss,no).
	
canon(T,ECs-Us-Ss,T1,ECs-Us-Ss,_) :-
	atomic(T), !,
	find(T,ECs,T1).
canon(T,ECs-Us-Ss,T2,ECs2-Us2-Ss2,SE) :-
	T=..[F|Ts],
	canon_args(Ts,ECs-Us-Ss,Ts1,ECs1-Us1-Ss1,SE),
	T1=..[F|Ts1],
	canonsig(T1,ECs1-Us1-Ss1,T2,ECs2-Us2-Ss2,SE).

canon_args([],ECs-Us-Ss,[],ECs-Us-Ss,_).
canon_args([T|Ts],ECs-Us-Ss,[T1|Ts1],ECs2-Us2-Ss2,SE) :-
	canon(T,ECs-Us-Ss,T1,ECs1-Us1-Ss1,SE),
	canon_args(Ts,ECs1-Us1-Ss1,Ts1,ECs2-Us2-Ss2,SE).

canonsig(T,ECs-Us-Ss,CanT,ECs-Us-Ss,_) :-
	T=..[_,T1|_],
	use(T1,Us,U_T1),
	member(U,U_T1),
	member(U-T,Ss), !,
	find(U,ECs,CanT).
canonsig(T,ECs-Us-Ss,T,ECs-Us2-Ss1,yes) :-
	T=..[_|Ts],
	graph_update(T,Ts,Us,Us1),
	ord_add_element(Us1,T-[],Us2),
	ordalist_set(Ss,T-T,Ss1).
canonsig(T,ECs-Us-Ss,T,ECs-Us-Ss,no) :-
	T=..[_|_].

graph_update(_,[],Us,Us).
graph_update(T,[T1|Ts],Us,Us1) :-
	use_select(T1-U_T1,Us,Us0),
	( member(T,U_T1) ->
	    graph_update(T,Ts,Us,Us1) ;
	    ord_add_element(U_T1,T,NewU_T1),
	    ord_add_element(Us0,T1-NewU_T1,Us2),
	    graph_update(T,Ts,Us2,Us1) ).

find(T,[],T).
find(T,[EC|_],CT) :- member(T,EC), member(CT,EC), !.
find(T,[_|ECs],RT) :- find(T,ECs,RT).

ec_union(A,B,ECs-Us-InEntEqs,NewECs-NewUs-OutEntEqs) :-
	( ec_select(A,ECs,ECA,ECs1) ->
	    ( member(B,ECA) ->
		NewECs=ECs, NewUs=Us, OutEntEqs=InEntEqs ;
		( ec_select(B,ECs1,ECB,ECs2) ->
		    ord_union(ECB,ECA,NewEC),
		    ECA=[RA|_], ECB=[RB|_], OutEntEqs=[RA=RB|InEntEqs],
		    ord_add_element(ECs2,NewEC,NewECs),
		    use_update(ECA,ECB,NewEC,Us,NewUs) ;
		    ECA=[RA|_], OutEntEqs=[RA=B|InEntEqs],
		    ord_add_element(ECA,B,ECAB),
		    ord_add_element(ECs1,ECAB,NewECs),
		    use_update(ECA,[B],ECAB,Us,NewUs)) ) ;
	    ( ec_select(B,ECs,ECB,ECs2) ->
		ECB=[RB|_], OutEntEqs=[A=RB|InEntEqs],
		ord_add_element(ECB,A,NewEC),
		ord_add_element(ECs2,NewEC,NewECs),
		use_update(ECB,[A],NewEC,Us,NewUs) ;
		OutEntEqs=[A=B|InEntEqs],
		list_to_ord_set([A,B],NewEC),
		ord_add_element(ECs,NewEC,NewECs),
		use_update([A],[B],NewEC,Us,NewUs)) ).

ec_select(T,ECs,EC,ECs1) :-
	select(EC,ECs,ECs1),
	member(T,EC), !.

use_update([A|_],[B|_],[C|_],Us,Us2) :-
	use(A,Us,UA),
	use(B,Us,UB),
	use_select(C-_,Us,Us1),
	( Us=Us1 ->
	    Us2=Us ;
	    ord_union(UA,UB,UC),
	    ord_add_element(Us1,C-UC,Us2) ).

ordalist_set([K-_|AL],K-V,[K-V|AL]) :- !.
ordalist_set([K1-V1|AL],K-V,[K1-V1|AL1]) :-
	K1 @< K, !,
	ordalist_set(AL,K-V,AL1).
ordalist_set(AL,K-V,[K-V|AL]).

myreplace_multi(_,_,[],[]).
myreplace_multi(A,B,[T|Ts],[T1|T1s]) :-
	myreplace(A,B,T,T1),
	myreplace_multi(A,B,Ts,T1s).
myreplace(A,B,A,B) :- !.
myreplace(_,_,T,T) :- atomic(T), !.
myreplace(A,B,T,T1) :-
	T=..[F|Args],
	myreplace_multi(A,B,Args,Args1),
	T1=..[F|Args1].
