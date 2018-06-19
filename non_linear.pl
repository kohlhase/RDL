:- use_module(library(lists)).
:- multifile method/6.
%lemma_speculation/6.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interfacing the decision procedure with the non-linear package %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- op(701,xfx,<--).

% Stripping the dependecy list from the inequality under consideration  
strip_dependency_list(SIneq<--_, SIneq). 

% Since we implement the affinization for x*y>=c and the
% standard form of the inequalities in the linear arithmetic
% decision procedure is the following: 
%          SUM(i=1, n, Ai*Xi) =< Const,  
% we must convert it, by means of the following identity:
%            (A>=B <==> -A=<-B)
% to 
%          SUM(i=1, n, (-Ai)*Xi) =< (-Const) 
adapt_inequality(P=<Const, OP>=OConst) :-
	Const=< 0,
	number_times_list(-1, P, OP),
	OConst is -1*Const.

% number_times_list(N, L, L1)
%  takes a number N and a list of monomials, i.e. terms of the form
%  C*T, and multiplies each one of them for N giving L1
number_times_list(N, [C*T|Rest], [C1*T|Rest1]) :-
	C1 is N*C,
	number_times_list(N, Rest, Rest1).
number_times_list(_, [], []).

splitcoeffsandmults([], [], []) :- !.
splitcoeffsandmults([N*M|T], [N|NRes], [M|MRes]) :-
	number(N),
	splitcoeffsandmults(T, NRes, MRes), !.

% get the list of the fringe of the multiplicand in a monomial
% of the polynomial and order such list
is_not_a_times_application(_*_) :- 
	!, fail.
is_not_a_times_application(_).

flatten(V, [V]) :- 
	is_not_a_times_application(V).
flatten(A*B, [A,B]) :- 
	is_not_a_times_application(A),
	is_not_a_times_application(B), !.

flatten(A*B, AtimesBlist) :- 
	flatten(A, A1),
	flatten(B, B1),
	append(A1, B1, AtimesBlist).

sortfringe(Xs, Ys) :- 
	permutation(Xs, Ys),
	orderedfringe(Ys), !.

orderedfringe([_]) :- !.
orderedfringe([X,Y|Ys]) :- 
	X @=< Y,  % cambiare con il predicato parametrico
		  % sull'ordering di RDL!
	orderedfringe([Y|Ys]).

sortfringes([], []) :- !.
sortfringes([X|T], [X1|T1]) :-
	sortfringe(X, X1),
	sortfringes(T, T1).

flattenmultandsortfringe(M, SF) :-
	flatten(M, X), 
	sortfringe(X, SF).

flattenmultlstandsortfringelst([], []) :- !.
flattenmultlstandsortfringelst([M|T], [SF|TSF]) :-
	flattenmultandsortfringe(M, SF),
	flattenmultlstandsortfringelst(T, TSF), !.

intersecttwolists([], _, []).
intersecttwolists([E|T], L, [E|I])	:-
	member(E, L),
	select(E, L, L1),
	intersecttwolists(T, L1, I), !.
intersecttwolists([E|T], L, I)	:-
	non_member(E, L),
	intersecttwolists(T, L, I), !.

intersectlistoflists([], []) :- !.
intersectlistoflists([L1,L2], IL1L2) :-
	intersecttwolists(L1, L2, IL1L2), !.
intersectlistoflists([L|Ls], Res) :-
	intersectlistoflists(Ls, Res1),
	intersecttwolists(L, Res1, Res), !.

find_max_common_factor(MonoLst, [X]) :-
	intersectlistoflists(MonoLst, XL),
	member(X, XL).

subtracttwolists(L, [], L).
subtracttwolists(L, [E|T], Res) :-
	select(E, L, L1),
	subtracttwolists(L1, T, Res), !.

subtractlistfromlistoflists([], _, []) :- !.
subtractlistfromlistoflists([Lst|LLst], L, [Res|ResLLst]) :-
	subtracttwolists(Lst, L, Res),
	subtractlistfromlistoflists(LLst, L, ResLLst), !.

calculate_remaining_factors(MonoLst, X, MonoLst1) :-
	subtractlistfromlistoflists(MonoLst, X, MonoLst1).

%%% TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% splitcoeffsandmults([3*(a*a*b*c*d), -2*(w*c)], _, Ms),        
% flattenmultlstandsortfringelst(Ms, MsLst), 
% find_max_common_factor(MsLst, X), 
% calculate_remaining_factors(MsLst, X, RemFs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list2term([], 1) :- !.
list2term([X], X) :- !.
list2term([X,Y], X*Y) :- !.
list2term([X|Lst], X*Res) :- 
	list2term(Lst, Res), !.

buildmults([], [], []) :- !.
buildmults([K|Ks], [M|Ms], [K*T|KsMs]) :-
	list2term(M, T),
	buildmults(Ks, Ms, KsMs), !.

%%% TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% splitcoeffsandmults([3*(a*a*b*c*d), -2*(w*c)], Ks, Ms),        
% flattenmultlstandsortfringelst(Ms, MsLst), 
% find_max_common_factor(MsLst, X), 
% calculate_remaining_factors(MsLst, X, RemFs),
% buildmults(Ks, RemFs, RemRes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluatenumericmults([], [], C, C) :- !.
evaluatenumericmults([A*B|T], T1, Const, NewConst) :-
	number(A), 
	number(B), 
	Const1 is Const+(A*B), 
	evaluatenumericmults(T, T1, Const1, NewConst),
	!.
evaluatenumericmults([A*B|T], [A*B|T1], Const, NewConst) :-
	number(A), 
	\+number(B), 
	evaluatenumericmults(T, T1, Const, NewConst),
	!.

%%% TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% splitcoeffsandmults([3*(a*a*b*c*d), -2*(w*c)], Ks, Ms),        
% flattenmultlstandsortfringelst(Ms, MsLst), 
% find_max_common_factor(MsLst, X), 
% calculate_remaining_factors(MsLst, X, RemFs),
% buildmults(Ks, RemFs, RemRes),
% evaluatenumericmults(RemRes, CorrectRemRes, 0, Const).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compactmultlst([], []) :- !.
compactmultlst([A*B|T], L) :-
	select(A1*B, T, T1),
	N is A+A1,
	compactmultlst([N*B|T1], L), !.
compactmultlst([_*B|T], L) :-
	non_member(_*B, T),
	compactmultlst(T, L), !.

%%% TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% splitcoeffsandmults([3*(a*a*b*c*d), -2*(a*a*b*d*c)], Ks, Ms),        
% flattenmultlstandsortfringelst(Ms, MsLst), 
% find_max_common_factor(MsLst, X), 
% calculate_remaining_factors(MsLst, X, RemFs),
% buildmults(Ks, RemFs, RemRes),
% evaluatenumericmults(RemRes, CorrectRemRes, 0, Const).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% splitcoeffsandmults([3*(a*c), -2*(c*a)], Ks, Ms),        
% flattenmultlstandsortfringelst(Ms, MsLst), 
% find_max_common_factor(MsLst, X), 
% calculate_remaining_factors(MsLst, X, RemFs),
% buildmults(Ks,RemFs, RemRes),
% evaluatenumericmults(RemRes, CorrectRemRes, 0, Const).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

buildmult([X], X) :- !.
buildmult([H|T], H*R) :-
	buildmult(T, R), !.


% returns a factorization of the input inequality in the form:
%                   X-(Y,K)-C
% s.t. X multiplied by Y+K is equal to the l.h.s. of the input inequality. 

factorize([N*(X*Y)]>=C, X-([N*Y],0)-C) :- 
	number(N), \+ number(X), \+ number(Y), number(C).
factorize(P>=C, X-(Y,C1)-C) :-
	number(C),
	splitcoeffsandmults(P, Ks, Ms),        
	flattenmultlstandsortfringelst(Ms, MsLst), 
	find_max_common_factor(MsLst, XLst), 
	calculate_remaining_factors(MsLst, XLst, RemFs), 
	buildmults(Ks, RemFs, RemRes),
	buildmult(XLst, X),
	evaluatenumericmults(RemRes, Y, 0, C1), 
	\+ number(X), \+ number(Y),
	Y\==[]. 
factorize(P>=C, X-(Y,0)-C) :-
	splitcoeffsandmults(P, Ks, Ms),        
	flattenmultlstandsortfringelst(Ms, MsLst), 
	find_max_common_factor(MsLst, XLst), 
	calculate_remaining_factors(MsLst, XLst, RemFs), 
	buildmults(Ks, RemFs, RemRes),
	buildmult(XLst, X1),
	evaluatenumericmults(RemRes, Y1, 0, _), 
	Y1==[], 
	X1=X*Y.

% factorize([N*(X*Y)|L]>=C, X-([N*Y],0)-0) :- 
% 	L\==[], number(N), \+ number(X), \+ number(Y), number(C).

%%% TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% factorize([3*(x*y)]=<3, X-Y-C).
% factorize([3*(a*c), -2*(c*a)]=<3, X-Y-C).
% factorize([3*(a*a*b*c*d), -2*(w*c)]=<9, X-Y-C).
% factorize([3*(w*a*a*b*c*d), -2*(w*c), 1*c]=<9, X-Y-C).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Affinizing inequalities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Preliminaries
% --------------
% Below, the function line( (x1, y1), (x2, y2) ) returns an expression
% that is zero along the straight line passing through the points (x1,
% y1) and (x2, y2), positive to the left of that line (as we move from
% (x1, y1) to (x2, y2)) and negative to the right of that line:
%     line( (x1, y1), (x2, y2) ) = 
%       = (x2-x1)*(y-y1)-(y2-y1)*(x-x1) 
%       = x2*y - x2*y1 - x1*y + x1*y1 - y2*x + y2*x1 + y1*x - y1*x1 
%       = (x2-x1)*y +(y1-y2)*x + (x1*y1-x2*y1+y2*x1-y1*x1) 
%       = (x2-x1)*y +(y1-y2)*x + (x1*y2-x2*y1) 
% Therefore, the corresponding polynomial inequality (i.e. the
% internal representation of the linear arithmetic decision procedure) 
% is:
%               [(x2-x1)*y,(y1-y2)*x]=<(x2*y1-x1*y2)
% if y is greater than x, 
%               [(y1-y2)*x,(x2-x1)*y]=<(x2*y1-x1*y2)
% otherwise.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a*x+b*y=c
line(X1-Y1, X2-Y2, A-B-C) :-
	A is Y1-Y2,
	B is X2-X1,
	C is X2*Y1-X1*Y2, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hyperbolic inequalities with positive constant 
% -----------------------------------------------
% To affinize the inequality x*y >= c, where c>=1, we break the domain
% of f(x)=c/x into two convex areas: 
%      x*y >= c >= 1 <==> (   (x =< -1 and y =< c/x)
%                          or (x >=  1 and y >= c/x) )
% Applying the affinization theorem to each area, we get that the
% inequality x*y >= c >= 1 is equivalent to:
%     ( x >=  1 and y >=  1 and 
%       AND(i=1, floor(sqrt(c)-1), 
%            line((i, floor(c/i)), (i+1, floor(c/(i+1)))) >= 0 and
%            line((floor(c/i), i), (floor(c/(i+1)), i+1)) =< 0) )
%  or ( x =< -1 and y =< -1 and 
%       AND(i=ceiling(1-sqrt(c)), -1, 
%            line((i, ceiling(c/i)), (i-1, ceiling(c/(i+1)))) >= 0 and
%            line((ceiling(c/i), i), (floor(c/(i-1)), i-1)) =< 0) )
%
% Hyperbolic inequalities with non-positive constant 
% ---------------------------------------------------
% The inequality x*y >= 0 is equivalent to:
%     ( x >= 0 and y >= 0 ) or ( x =< 0 and y =< 0 )
% If c =< -1, then the inequality x*y >= c describes the non-convex
% area between two hyperbola branches.  We transform this inequality
% to negation of positive-constant hyperbolic inequality:
%            x*y >= c <==> not(x'*y' >= c') 
% where x' = -x and y' = y, and c' = 1-c>=1.
% Negation of conjunct produces disjunction of several constraints. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
processpair(([Y],0), Y):- !.
%processpair((Y,0), Y):- !.
processpair(([Y],K), Y+K):- !.
processpair((Y,0), AY):-
	length(Y,YL), YL>1,
	list2add(Y, AY), !.
processpair((Y,K), AY+K):-
	K>0,
	length(Y,YL), YL>1,
	list2add(Y, AY), !.

list2add([E], E).
list2add([E1,E2], E1+E2):- !.
list2add([E1|ELS], E1+Res):-
	list2add(ELS, Res).
% affine_hyperbolic_inequality(X-Y-Const,
%                              AffinizedHyperbolicConstraint)
% The first argument of the predicate is a compact description of the
% inequality we are going to affinize: 
%                            X-Y-Const
% where X and Y are the variable uniquely determining the plane in
% which the hyperbolic constraint lies, Const is the constant term.
% The second argument is the result of the affinization, i.e. a list
% of affine constraints, whose conjunction is logically equivalent
% (over the naturals) to the original non-affine constraint described
% by X-Y-Const-DL, under the assumption that X >= 0 and Y >= 0.  Thus
% we need to check whether these two conditions hold by CCR! 
% The code below considers two cases: c = 0   and  c =/= 0.

% This is the trivial case: c = 0. 
% In this case we want to affinize something like x*y =< 0, then we
% want the signs of x and y to be opposite, hence we have two
% sub-cases, either X is positive and Y is negative, or X is negative
% and Y is positive.  These two sub-cases are encoded by means of the
% or below.
affine_hyperbolic_inequality(X-Y-0, [X>=0]-[Y1>=0]) :-
	processpair(Y, Y1).
affine_hyperbolic_inequality(X-Y-0, [X=<0]-[Y1=<0]) :-
	processpair(Y, Y1).
affine_hyperbolic_inequality(X-Y-0, [Y1>=0]-[X>=0]) :-
	processpair(Y, Y1).
affine_hyperbolic_inequality(X-Y-0, [Y1=<0]-[X=<0]) :-
	processpair(Y, Y1).

affine_hyperbolic_inequality(X-Y-1, [X>=1]-[Y1>=1]) :-
	processpair(Y, Y1).
affine_hyperbolic_inequality(X-Y-1, [X=< -1]-[Y1=< -1]) :-
	processpair(Y, Y1).
affine_hyperbolic_inequality(X-Y-1, [Y1>=1]-[X>= 1]) :-
	processpair(Y, Y1).
affine_hyperbolic_inequality(X-Y-1, [Y1=< -1]-[X=< -1]) :-
	processpair(Y, Y1).

affine_hyperbolic_inequality(X-Y- -1, [X>=1]-[Y1=< -1]) :-
	processpair(Y, Y1).
affine_hyperbolic_inequality(X-Y- -1, [X=< -1]-[Y1>= 1]) :-
	processpair(Y, Y1).
affine_hyperbolic_inequality(X-Y- -1, [Y1>=1]-[X=< -1]) :-
	processpair(Y, Y1).
affine_hyperbolic_inequality(X-Y- -1, [Y1=< -1]-[X>= 1]) :-
	processpair(Y, Y1).

% This is the non-trivial case: c >= 1.  We have two sub-cases: either
% X,Y>=0 or X,Y=<0.  These two sub-cases are encoded in the two
% predicates pos_affine_hyperbolic_inequality and
% neg_affine_hyperbolic_inequality below. 

eqline(X1-Y1, X2-Y2, _-(_,K), A-B-CminusBK) :-
	line(X1-Y1, X2-Y2, A-B-C),
	CminusBK is C - B*K.

buildlhs(0, _, 0, _, 0) :- !.
buildlhs(0, _, B, Y, B*Y) :- !.
buildlhs(A, X, 0, _, A*X) :- !.
buildlhs(A, X, B, Y, A*X+B*Y) :- !.	

new_gen_lines1(X-(Y,K)-C, I, [LHS>=Const,LHSP=<ConstP|Rest]) :-
	YI is integer(ceiling(C/I)),
        Iplus1 is I+1,
	YIplus1 is integer(ceiling(C/Iplus1)),
	eqline(I-YI, Iplus1-YIplus1, X-(Y,K), A-B-Const),
	processpair((Y,K), YP),
	buildlhs(A, X, B, YP, LHS),
	eqline(YI-I, YIplus1-Iplus1, X-(Y,K), AP-BP-ConstP),
	buildlhs(AP, X, BP, YP, LHSP),
	Max is integer(ceiling(sqrt(C)-1)),
	I =< Max,
	new_gen_lines1(X-(Y,K)-C, Iplus1, Rest).
new_gen_lines1(_-(_,_)-C, Max, []) :-
	Max1 is integer(ceiling(sqrt(C)-1)),
	Max > Max1.


new_gen_lines2(X-(Y,K)-C, I, [LHS>=Const,LHSP=<ConstP|Rest]) :-
	I =\= 0,
	YI is integer(floor(C/I)),
	Iminus1 is I-1,
	YIminus1 is integer(floor(C/Iminus1)),
	eqline(I-YI, Iminus1-YIminus1, X-(Y,K), A-B-Const),
	processpair((Y,K), YP),
	buildlhs(A, X, B, YP, LHS),
	eqline(YI-I, YIminus1-Iminus1, X-(Y,K), AP-BP-ConstP),
	buildlhs(AP, X, BP, YP, LHSP),
	I =< -1,
        Iplus1 is I+1, new_gen_lines2(X-(Y,K)-C, Iplus1, Rest).
new_gen_lines2(_-(_,_)-_, 0, []) :- !.


affine_hyperbolic_inequality(X-Y-C, [X>=1,Y1>=1]-[X>=1,Y1>=1|Res]) :- 
	C>1,
	processpair(Y,Y1),
	new_gen_lines1(X-Y-C, 1, Res).
	
affine_hyperbolic_inequality(X-Y-C, [X=< -1,Y1=< -1]-[X=< -1,Y1=< -1|Res]) :- 
	C>1,
	processpair(Y,Y1),
	START is integer(floor(1-sqrt(C))),
	new_gen_lines2(X-Y-C, START, Res).

affinize(What, Res) :-
	affine_hyperbolic_inequality(What, Res).
%	format("\n ~w : ~w\n", [What, Res]).

%%% TO BE GENERALIZED TO ARBITRARY EVEN POWERS? 
drop_pos_power(P=< Const, TermRest=< Const) :-
	P=[N*Mult|Rest],
	( Mult=X*(X*(X*X)) ; Mult=X*X ),
	N>0,
	list2add(Rest, TermRest).
%	format("\n ~w --> ~w \n", [P=< Const, Rest=< Const]).





