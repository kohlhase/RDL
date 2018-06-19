%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Multiset orders %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Multisets are sets with repeated elements.  Examples
% are: {a, a, b} and {a, b, a} which are identical and {a, b, b} which
% is distinct from them. 
%
% DEFINITION. A multiset M over a set A is a function M : A --> Nat. 
% Inuitively, M(X) is the number of copies of x (in A) in M.  A
% multiset M is finite if there are only finitely many x
% s.t. M(x)>0. Finally, let M(A) denote the set of all finite
% multisets over A.  In the following, we use standard set notation
% like {a,a,b} as an abbreviation of the function {a |-> 2, b |-> 1, c
% |-> 0 } over the set A = {a, b, c}. 
% Most set operations are easily generalized to multisets by replacing
% the underlying boolean operation by similar ones on Nat, e.g.:
%  ELEMENT:    x in M       :<===> M(x)>0
%  INCLUSION:  M subseteq N :<===> forall x in A. M(x) <= N(x)
%  UNION:      (M cup N)(x) :=     M(x)+N(x)
%  DIFFERENCE: (M \ N)(x)   :=     M(x)--N(x), where -- denotes the
%                                              "cut-off" subtraction
%                                              operation 
%
% CENTRAL CONCEPT: an order on multisets, the smaller multiset is
%                  obtained from the larger one by removing a
%                  non-empty subset X and adding only elements which
%                  are smaller than some element in X.
% DEFINITION: 
%     M >mul N :<===> there exist X, Y in M(A) s.t.
%                        emptyset =/= X subseteq M and
%                        N = (M - X) cup Y         and
%                        forall y in Y. exists x in X s.t. x > y 
%
% For example, {5,3,1,1}>mul{4,3,3,1} is verified by replacing X =
% {5,1} by Y = {4,3}. Note that X and Y are not uniquely determined: X
% = {5,3,1,1} and Y = {4,3,3,1} work just as well.
% Sometimes it can be useful to realize that M>mul N holds iff one can
% get from M to N by carrying out the following procedure one or more
% times: remove an element from x and add a finite number of elements,
% all of which are smaller than x. 
% On finite multisets, the multiset order is again a strict order.
% But, the really important non-trivial property of >mul is that the
% multiset order >mul is well-founded iff > is.
%
% The above definition of >mul is quite intuitive but also cumbersome
% because of its rich first-order structure, therefore the following
% alternative characterization is useful.
% LEMMA: if > is a strict order on A and M, N in M(A), then 
%          M >mul N  <===> M =/= N and
%                          forall n in (N-M).
%                           exists m in (M-N). m>n
%
% It is worth noting that if > is linear, then M>mul N can be computed
% quite efficiently: sort M and N into descending order (w.r.t. >) and
% compare the resulting lists lexicographically w.r.t. >Lex.  Let &M
% be the sorted version of M.  
%
% The multiset extension of a partial order >= is defined as follows:
%                M >=mul N :<===> M >mul N or M = N
% and we do not simply replace > with >= in the above definition,
% since we would obtain the undesirable {1} >=mul {1,1}

%%% Implementation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%
% First of all, we implement strict/partial orders:
% gr -> greater-than, eq -> equal-to and nge -> less-than
ord(_-_-prolog, X, Y, gr) :- X @> Y, !.
ord(_-_-prolog, X, Y, eq) :- X == Y, !.
ord(_-_-prolog, X, Y, nge):- X @< Y.
ord(T-Ex-weight,X,Y,R) :-
	weight(X,XW,T-Ex-weight),
	weight(Y,YW,T-Ex-weight),
	( XW=:=YW ->
	    R=eq ;
	    ( XW>YW -> R=gr ; R=nge ) ).
ord(T-Ex-kbo,X,Y,R) :-
	weight(X,XW,T-Ex-kbo),
	weight(Y,YW,T-Ex-kbo),
	( XW>YW -> R=gr ;
	    ( XW<YW -> R=nge ;
		X=..[FX|ArgsX],
		Y=..[FY|ArgsY],
		( ord_gt(Ex,T,FX,FY) -> R=gr ;
		    ( ord_gt(Ex,T,FY,FX) -> R=nge ;
			ord_lex(T-Ex-kbo,ArgsX,ArgsY,R) ) ) ) ).
ord(_-_-none, _, _, _).

ord_lex(_-_-_,[],[],eq) :- !.
ord_lex(_-_-_,[_|_],[],gr) :- !.
ord_lex(_-_-_,[],[_|_],nge) :- !.
ord_lex(T-Ex-Ord,[X|Xs],[Y|Ys],R) :-
	ord(T-Ex-Ord,X,Y,R0),
	( R0=eq ->
	    ord_lex(T-Ex-Ord,Xs,Ys,R) ;
	    R=R0 ).

% Notice that we use the built-in Prolog ordering over terms.
% See Chap 7, page 107 of the SICStus manual for a definition of this
% ordering. 

% We represent finite multisets with lists, which leads to very simple
% algorithms (for example, cup becomes list concatenation).  
rem1([], _,[],_).
rem1([X|Xs],Y,Xs,Pb) :- ord(Pb,X, Y, eq), !.
rem1([X|Xs],Y,[X|Ts],Pb) :- rem1(Xs, Y, Ts,Pb).

mdiff(Xs,[],Xs,_).
mdiff(Xs,[Y|Ys],Res,Pb) :- rem1(Xs,Y,W,Pb), mdiff(W,Ys,Res,Pb).

% The following implementation of >mul is not derived from the
% DEFINTION above, it is rather inspired to the LEMMA, which allows
% for a more operational reading:
allex([],_,_).
allex([N|T],Mlst,Pb):- ex(Mlst,N,Pb), allex(T,Mlst,Pb).

ex([M|_],N,Pb):- ord(Pb,M,N,gr), !.
ex([_|T],N,Pb):- ex(T,N,Pb).

msgr(Pb,Ms,Ns) :-
	mdiff(Ns, Ms, Nms,Pb),
	mdiff(Ms, Ns, Mns,Pb),
	Mns=[_|_],
	allex(Nms, Mns, Pb).
msgr(_-_-none,_,_).

tsize(T,1) :- atomic(T), !.
tsize(T,N) :-
	T=..[_|Ts],
	tsize_multi(Ts,M),
	N is M+1.

tsize_multi([],0).
tsize_multi([T|Ts],M) :-
	tsize(T,N),
	tsize_multi(Ts,Ns),
	M is N+Ns.

weight(T,N,Pb) :-
	atomic(T), !,
	symbol_weight(Pb,T,N).
weight(T,N,Pb) :-
	T=..[F|Ts],
	symbol_weight(Pb,F,FN),
	weight_multi(Ts,Ns,Pb),
	N is FN+Ns.

weight_multi([],0,_).
weight_multi([T|Ts],N1,Pb) :-
	weight(T,N,Pb),
	weight_multi(Ts,Ns,Pb),
	N1 is N+Ns.

symbol_weight(T-Ex-weight,F,W) :-
	symbol_weight(Ex,T,F,W), !.
symbol_weight(_-_-weight,_,1).
symbol_weight(T-Ex-kbo,F,W) :-
	symbol_weight(Ex,T,F,W), !.
symbol_weight(_-_-kbo,_,1).

	