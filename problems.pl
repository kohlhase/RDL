%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% RDL Examples %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Description %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% description(
% 	Name,               % Name of the Problem: a Prolog atom.
% 	Author,             % Author of the problem: a string
% 	Description         % Description of the problem: a string
%     ).

%%% Facts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fact(Problem,     % Name of the problem
%      Name,        % Name of the fact
%      Prems,       % List of premises of the fact
%      Concl).      % Conclusion of the fact

% pred_sym(Name,          % Name of the Problem
%          Pappl)         % Prolog term of the form P(_, _, ..., _)
%                         % where P is an interpreted predicate symbol.

%%% Input & Output formulae %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% input(Name,   % Name of the problem
%       Phi).   % The formula to be simplified

% expected_output(Name,    % Name of the problem
%                 Version, % Instance of the system
%                 Order,   % Ordering to be used.
%                          % Currently supported: prolog and none.
%                 Phi).    % The formula expected as a result from 
                           % the simplification activity of RDL.
	 
%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(trivial, 
	    'Alessandro Armando', 
	    'Very easy, just for testing.').
input(trivial, [not(x=y), f(x)=f(y)]).
expected_output(trivial, eq, prolog, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(zhang,
'Alessandro Armando',
'The following example comes from H. Zhang paper on Contextual Rewriting.').
fact(zhang, rem_times, [not(U=0)],rem(U*_,U)=0).
input(zhang, [rem(y*z,x)=0,not(x*y=y*z),x=0]).
ord_gt(zhang,_,y,x).
expected_output(zhang, eq, kbo, [true]).    
expected_output(zhang, la, kbo, [true]).    
expected_output(zhang, aug(eq), kbo, [true]).    
expected_output(zhang, aug(la), kbo, [true]).    
expected_output(zhang, eq_la, kbo, [true]).    
expected_output(zhang, aug(eq_la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(sqrt,
'Alessandro Armando',
'').
fact(sqrt, sqrt_squared, [X>=0],sqrt(X*X)=X).
input(sqrt, [not(n>=0), not(n*n=exp(2,n)),sqrt(exp(2,n))=n]).
symbol_weight(sqrt,_,exp,2).
expected_output(sqrt, eq, kbo, [true]).    
expected_output(sqrt, aug(eq), kbo, [true]).    
expected_output(sqrt, la, kbo, [true]).    
expected_output(sqrt, aug(la), kbo, [true]).    
expected_output(sqrt, eq_la, kbo, [true]).    
expected_output(sqrt, aug(eq_la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(sqrt1,
'Alessandro Armando',
'').
fact(sqrt1, sqrt_squared, [X>=0],sqrt(X*X)=X).
input(sqrt1, [not(n>=0), not(n*n-exp(2,n)>=0), not(n*n=<exp(2,n)), sqrt(exp(2,n))=n]).
symbol_weight(sqrt1,_,exp,2).
expected_output(sqrt1, la, kbo, [true]).    
expected_output(sqrt1, aug(la), kbo, [true]).    
expected_output(sqrt1, eq_la, kbo, [true]).    
expected_output(sqrt1, aug(eq_la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(sqrt2,
'Alessandro Armando',
'').
fact(sqrt2, sqrt_squared, [X>=0],sqrt(X*X)=X).
fact(sqrt2, sqr_lt_exp, [X>=4],X*X=<exp(2,X)).
input(sqrt2, [not(n>=5), not(n*n-exp(2,n)>=0), sqrt(exp(2,n))=n]).
symbol_weight(sqrt2,_,exp,2).
expected_output(sqrt2, aug(la), kbo, [true]).    
expected_output(sqrt2, aug(eq_la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(aug1,
'Alessandro Armando',
'The following exploits augmentation of the congruence
closure graph and therefore should fall beyond the 
capabilities of H. Zhang contextual rewriting. 
It should be tackled by Kapur schema though.').
input(aug1, [x*y+z=y*x+z]).
expected_output(aug1, eq, kbo, [x*y+z=y*x+z]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(aug2,
'Alessandro Armando',
'The following exploits augmentation of the congruence
closure graph and therefore should fall beyond the 
capabilities of H. Zhang contextual rewriting. 
It should be tackled by Kapur schema though.').
input(aug2, [x*y+z=y*x+k]).
expected_output(aug2, eq, kbo, [x*y+z=y*x+k]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(subs_eq,
'Alessandro Armando',
'Clause simplification via subsumtion for Contextual Rewriting.').
input(subs_eq, [not(a=b),not(f(a)=f(b))]). 
expected_output(subs_eq, eq, kbo, [not(a=b)]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(shankar1,
'Alessandro Armando',
'Stretching the congruence closure engine (from Shankar et al. CADE paper)').
input(shankar1, [not(x=y),not(f(f(f(x)))=f(x)),f(f(f(f(f(y)))))=f(x)]).
expected_output(shankar1, eq, kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(frocos2000,
'Silvio Ranise',
'This example is taken from "Termination of Constraint Contextual Rewriting",
FroCos2000, Example 1, page 3').
fact(frocos2000, unique_rr, [f(U)=f(V)], r(g(U,V),U)=U).
input(frocos2000, [r(g(y,z),x)=x,not(g(x,y)=g(y,z)),not(y=x)]).
ord_gt(frocos2000,_,y,x).
expected_output(frocos2000, eq, kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(eq_aug_zhang1,
'Alessandro Armando',
'Variant of zhang which requires augmentation.').
fact(eq_aug_zhang1, comm_times, [],X*Y=Y*X).
fact(eq_aug_zhang1, rem, [not(U=0)],rem(U*_,U)=0).
input(eq_aug_zhang1, [rem(y*z,x)=0,not(x*y=z*y),x=0]).
ord_gt(eq_aug_zhang1,_,z,y).
ord_gt(eq_aug_zhang1,_,y,x).
expected_output(eq_aug_zhang1, eq, kbo, [true]).
expected_output(eq_aug_zhang1, aug(eq), kbo, [true]).
expected_output(eq_aug_zhang1, la, kbo, [true]).
expected_output(eq_aug_zhang1, aug(la), kbo, [true]).
expected_output(eq_aug_zhang1, eq_la, kbo, [true]).
expected_output(eq_aug_zhang1, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(eq_aug_zhang2,
'Alessandro Armando',
'Variant of zhang which requires augmentation.').
fact(eq_aug_zhang2, comm_times, [],X*Y=Y*X).
input(eq_aug_zhang2, [x*y+z=y*x+k]).
ord_gt(eq_aug_zhang2,_,y,x).
expected_output(eq_aug_zhang2, aug(eq), kbo, [x*y+z=x*y+k]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(zhang_la,
'Alessandro Armando',
'This comes from H. Zhang paper on Contextual Rewriting.
Notice that ordering MUST be disabled.').
fact(zhang_la, rem_rew, [U>0],rem(U*_,U)=0).
input(zhang_la, [rem(x*y,x)=0,x=<0]).
expected_output(zhang_la, la, kbo, [true]).
expected_output(zhang_la, aug(la), kbo, [true]).
expected_output(zhang_la, aug(eq_la), kbo, [true]).
	
%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(zhang_la_bis,
'Silvio Ranise',
'A modified version of zhang_la which copes with the ordering of
Prolog terms.').
fact(zhang_la_bis, rem_rew, [not(U=<0)],rem(U*_,U)=0).
input(zhang_la_bis, [rem(x*y,x)=0,x=<0]).
expected_output(zhang_la_bis, la, kbo, [true]).
expected_output(zhang_la_bis, aug(la), kbo, [true]).
expected_output(zhang_la_bis, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(subs_la,
'Alessandro Armando',
'Clause simplification via subsumtion for Conditional
Rewriting and a Linear Arithmetic Decision Procedure.').
input(subs_la, [not(d>b),not(d>=b)]).
expected_output(subs_la, la, kbo, [not(d>b)]).
expected_output(subs_la, aug(eq_la), kbo, [not(d>b)]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(trans1,
'Alessandro Armando',
'Transitivity and anti-simmetry').
input(trans1, [not(d>b+1),not(b>c+3),d>c]).
expected_output(trans1, la, kbo, [true]).
expected_output(trans1, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(trans2,
'Alessandro Armando',
'Again transitivity and anti-simmetry').
input(trans2, [not(d>b),not(b>c),d-1>c]).
expected_output(trans2, la, kbo, [true]).
expected_output(trans2, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(trans3,
'Alessandro Armando',
'And again transitivity and anti-simmetry').
input(trans3, [not(d>b),not(b>c),d-2>c]).
expected_output(trans3, la, kbo,     [not(b>c),not(d>b),d-2>c]).
expected_output(trans3, aug(eq_la), kbo, [not(b>c),not(d>b),d-2>c]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(frocos2000_la,
'Silvio Ranise',
'Example 2 at page 3, "Termination of Constraint Contextual Rewriting" 
with a Linear Arithmetic Decision Procedure and not with the one for
Linear Orders as discussed in the paper.').
fact(frocos2000_la, unique_rew, [g(U,V)=<U], r(g(U,V),U)=U).
input(frocos2000_la, [r(g(y,z),x)=x, not(x>g(y,z)), 
                      not(g(x,y)=<g(y,z)), not(g(x,y)>=g(y,z))]).
ord_gt(frocos2000_la,_,y,x).
expected_output(frocos2000_la, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(bm94_simplified,
'Silvio Ranise',
'This is a simplified version of an example in the paper "Integrating
Decision Procedures in Heuristic Theorem Provers: ..." by Boyer and
Moore, at page 94.').
input(bm94_simplified, [not(min(d)=<max(d)),not(k>=0),
 	                not(l>=0), not(l=<min(d)), not(0<k), 
			l<max(d)+k]).
expected_output(bm94_simplified, la, kbo, [true]).
expected_output(bm94_simplified, aug(eq_la), kbo, [true]).

	
%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(ramsey_rule-N-A,
'Silvio Ranise',
'This is a lemma taken from H. Zhang paper about Ramsey theorem.') :-
	number(N), number(A).
%%% ramsey_rule37: 0>=x == x=0
input(ramsey_rule-37-1, [x=0, not(0>=x), not(x>=0)]).
expected_output(ramsey_rule-37-1, la, kbo, [true]).
input(ramsey_rule-37-2, [0>=x, not(x=0), not(x>=0)]).
expected_output(ramsey_rule-37-2, la, kbo, [true]).
%%% ramsey_rule38: (y+z)>=x if (y>=x)
input(ramsey_rule-38-0, [y+z>=x, not(y>=x), not(x>=0), not(y>=0), not(z>=0)]).
expected_output(ramsey_rule-38-0, la, kbo, [true]).
%%% ramsey_rule41: (x>=y) if (suc(x+u)>= (y+v) and (not(u>=v)))
input(ramsey_rule-41-0, [x>=y, not(x+u+1>=y+v), u>=v]).
expected_output(ramsey_rule-41-0, la, kbo, [true]).
% %%% ramsey_rule43: (y>=suc(x)) == not(x>=y)
input(ramsey_rule-43-1, [y>=x+1, x>=y]).
expected_output(ramsey_rule-43-1, la, kbo, [true]).
input(ramsey_rule-43-2, [not(y>=x+1), not(x>=y)]).
expected_output(ramsey_rule-43-2, la, kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(bm94,
'Alessandro Armando',
'This is an example taken from the paper "Integrating Decision
Procedures in Heuristic Theorem Provers: ..." by Boyer and
Moore, page 94.  See bm94_simplified for a simplified version
which does not need augmentation.').
fact(bm94, min_lesseq_max, [], min(L)=<max(L)).
input(bm94, [not(k>=0), not(l>=0), not(l=<min(b)), not(0<k), l<max(b)+k]).
expected_output(bm94, aug(la), kbo, [true]).
expected_output(bm94, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(bm95,
'Alessandro Armando',
'This is an example taken from the paper "Integrating Decision
Procedures in Heuristic Theorem Provers: ..." by Boyer and
Moore, page 95.  Notice that we must disable the ordering.').
pred_sym(bm95, memb(_,_)).
fact(bm95, len_del_less_len, [memb(X,S)], len(del(X,S))<len(S)).
input(bm95, [not(w>=0), not(k>=0), not(z>=0), not(v>=0),
             not(memb(z,b)),not(w+len(b)=<k), w+len(del(z,b))<k+v]). 
expected_output(bm95, aug(la), kbo, [true]).
expected_output(bm95, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(bm101,
'Alessandro Armando',
'This is an example taken from the paper "Integrating Decision
Procedures in Heuristic Theorem Provers: ..." by Boyer and
Moore, page 101.').
fact(bm101, delta1_lesseq_lp, [], delta1(_,LP,_)=<LP).
input(bm101, [not(lp+lt=<maxint), not(i=<lt),
	      i+delta1(pat,lp,c)=<maxint]).
% expected_output(bm101, eq, kbo, [true]).
% expected_output(bm101, la, kbo, [true]).
% expected_output(bm101, aug(eq), kbo, [true]).
expected_output(bm101, aug(la), kbo, [true]).
expected_output(bm101, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(bm102_simplified,
'Alessandro Armando',
'This is an example taken from the paper "Integrating Decision
Procedures in Heuristic Theorem Provers: ..." by Boyer and
Moore, page 102.  This is a nice examples of what augmentation can
do: a non linear formula is successfully simplified by a linear
arithmetic decision procedure if this is helped by augmentation!
Notice that we must associate the nested multiplications to the right 
(in Kbo * associates to the left), otherwise augment
fails to find the right instances of rule arith_fact. 
This would not be necessary if we had A-matching.').
fact(bm102_simplified, arith_fact, [0<I],J=<I*J).
fact(bm102_simplified, ms_fact, [],0<ms(_)).
input(bm102_simplified, [0 < ms(d)*ms(b)]).
% expected_output(bm102_simplified, eq, kbo, [true]).
% expected_output(bm102_simplified, la, kbo, [true]).
% expected_output(bm102_simplified, aug(eq), kbo, [true]).
expected_output(bm102_simplified, aug(la), kbo, [true]).
expected_output(bm102_simplified, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(bm102,
'Alessandro Armando',
'This is an example taken from the paper "Integrating Decision
Procedures in Heuristic Theorem Provers: ..." by Boyer and
Moore, page 102.  This is a nice examples of what augmentation can
do: a non linear formula is successfully simplified by a linear
arithmetic decision procedure if this is helped by augmentation!
Notice that we must associate the nested multiplications to the right 
(in Kbo * associates to the left), otherwise augment
fails to find the right instances of rule arith_fact. 
This would not be necessary if we had A-matching.').
fact(bm102, arith_fact, [0<I],J=<I*J).
fact(bm102, ms_fact, [],0<ms(_)).
input(bm102, [ms(c)+ms(d)*ms(d)+ms(b)*ms(b)<
               ms(c)+ms(b)*ms(b)+2*(ms(d)*ms(d))*ms(b)+
                ms(d)*(ms(d)*(ms(d)*ms(d)))]).
% expected_output(bm102, eq, kbo, [true]).
% expected_output(bm102, la, kbo, [true]).
% expected_output(bm102, aug(eq), kbo, [true]).
expected_output(bm102, aug(la), kbo, [true]).
expected_output(bm102, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(bm102_bis,
'Alessandro Armando',
'This is a modified version of an example taken from the paper
"Integrating Decision Procedures in Heuristic Theorem Provers: ..." by
Boyer and Moore, page 102.  We change the whay the term
2*ms(a)*ms(a)*ms(b) is associated and we get a slightly longer proof.').
fact(bm102_bis, arith_fact, [0<I],J=<I*J).
fact(bm102_bis, ms_fact, [],0<ms(_)).
input(bm102_bis, [ms(c)+ms(d)*ms(d)+ms(b)*ms(b)<
                   ms(c)+ms(b)*ms(b)+2*(ms(d)*(ms(d)*ms(b)))+
                    ms(d)*(ms(d)*(ms(d)*ms(d)))]).
expected_output(bm102_bis, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(frocos2000_aug,
'Silvio Ranise',
'This is Example 4, page 9 in the paper "Termination of CCR",
FroCos2000.  As for bm102 (and its variant  bm102_bis), this is a nice
example of the power of the augmentation heuristics: an inequality
containing the transcendental function exp(.) is successfully decided
by the "cooperation" of a linear arithmetic decision procedure and the
augmentation heuristics!  Notice that we must disable the ordering.').
fact(frocos2000_aug, sqr_lesseq_exp, [X>=4], X*X=<exp(2,X)).
input(frocos2000_aug, [not(c>=4), not(b=<c*c), not(exp(2,c)<b)]).
expected_output(frocos2000_aug, aug(la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(kapur_nie_11,
'Alessandro Armando',
'Kapur example at pag. 11 of the paper "Reasoning about numbers in Tecton".').
pred_sym(kapur_nie_11,p(_)).
fact(kapur_nie_11, min_rew, [max(X,Y)=X], min(X,Y)=Y).
fact(kapur_nie_11, f_lesseq_g, [p(X)], f(X)=<g(X)).
input(kapur_nie_11, [not(p(x)), not(z=<f(max(x,y))), not(0<min(x,y)),
                     not(x=<max(x,y)), not(max(x,y)=<x), z<g(x)+y]).
expected_output(kapur_nie_11, aug(la), kbo, [true]).
expected_output(kapur_nie_11, eq_la, kbo, [true]).
expected_output(kapur_nie_11, aug(eq_la), kbo, [true]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Non Linear Problems %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-0, 
'Silvio Ranise', 
'Without x>0 in the context, affinization would not be able to solve
 this problem.  This shows the importance of the context in which
 proof obligations are proved.  See also problem nl-0-bis.').
input(nl-0, [not(x>0), x*x>0]).
expected_output(nl-0, aff(la), kbo, [true]).    
expected_output(nl-0, aff(eq_la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-0-bis, 
'Silvio Ranise', 
'Without x<0 in the context, affinization would not be able to solve
 this problem.  This shows the importance of the context in which
 proof obligations are proved.  See also problem nl-0.').
input(nl-0-bis, [not(x<0), x*x>0]).
expected_output(nl-0-bis, aff(la), kbo, [true]).    
expected_output(nl-0-bis, aff(eq_la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-1, 
'Silvio Ranise', 
'This is a basic property about the sign of a product: the product of
 two (strictly) positive integers is a (strictly) positive integer.').
input(nl-1, [not(x>0), not(y>0), x*y>0]).
expected_output(nl-1, aff(la), kbo, [true]).    
expected_output(nl-1, aff(eq_la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-1-bis, 
'Silvio Ranise', 
'This is a weaker form of the proof obligation in problem
 non-linear-1.'). 
input(nl-1-bis, [not(x>= 1), not(y>= 1), x*y>= 1]).
expected_output(nl-1-bis, aff(la), kbo, [true]).    
expected_output(nl-1-bis, aff(eq_la), kbo, [true]).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Examples from Bjorner's Phd thesis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-1-bjorner, 
'Silvio Ranise', 
'This is taken from N. Bjorner PhD Thesis (page 72, Example 1 in Table
 5.1).'). 
input(nl-1-bjorner, 
       [not(m=<l+d*r), not(r<0), not(x+t=<d), not(t>0), m=<l+r*x+r*t]).
expected_output(nl-1-bjorner, aff(la), prolog, [true]).    
expected_output(nl-1-bjorner, aff(eq_la), prolog, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-example-bjorner, 
'Silvio Ranise', 
'This is taken from N. Bjorner PhD Thesis (Example at page 76).').
input(nl-example-bjorner, 
	[not(x>0), not(y>0), not(z>0), x*y*z+x*y*z*z>0]).
expected_output(nl-example-bjorner, aff(la), kbo, [true]).    
expected_output(nl-example-bjorner, aff(eq_la), kbo, [true]).    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Examples from Harrison's Phd thesis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-1-harrison, 
'Silvio Ranise', 
'This is taken from Harrison PhD Thesis (page 89, 1st example in the
 Table of Section 5.6).  We added the hypothesis x>0 (see also
 nl-0 and nl-0-bis for the importance of the context
 in which proof obligations are proved).').
input(nl-1-harrison, [not(x>0), not(x*x-x+1=0)]).
expected_output(nl-1-harrison, aff(la), kbo, [true]).    
expected_output(nl-1-harrison, aff(eq_la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-1-bis-harrison, 
'Silvio Ranise', 
'This is taken from Harrison PhD Thesis (page 89, 1st example in the
 Table of Section 5.6).  We added the hypothesis x<0 (see also
 nl-0 and nl-0-bis for the importance of the context
 in which proof obligations are proved).').
input(nl-1-bis-harrison, [not(x<0), not(x*x-x+1=0)]).
expected_output(nl-1-bis-harrison, aff(la), kbo, [true]).    
expected_output(nl-1-bis-harrison, aff(eq_la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-2-harrison, 
'Silvio Ranise', 
'This is taken from Harrison PhD Thesis (page 89, 3rd example in the
 Table of Section 5.6).').
input(nl-2-harrison, [not(x>6),not(x*x-3*x+1=0)]).
expected_output(nl-2-harrison, aff(la), kbo, [true]).    
expected_output(nl-2-harrison, aff(eq_la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-4-bis-harrison, 
'Silvio Ranise', 
'This is taken from Harrison PhD Thesis (page 89, 1st example after the
 Table of Section 5.6).  We added the hypothesis x<0 (see also
 nl-0 and nl-0-bis for the importance of the context
 in which proof obligations are proved).').
input(nl-4-bis-harrison, [not(x<0),
                              not(11*x*x*x-7*x*x-2*x+1=0),
                              not(7*x*x-5*x+3>0), 
                              not(x*x-8*x+1=0)]).
expected_output(nl-4-bis-harrison, aff(la), kbo, [true]).    
expected_output(nl-4-bis-harrison, aff(eq_la), kbo, [true]).    

%%% Examples from the PVS Prelude %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% both_sides_times_pos_lt1: LEMMA x * pz < y * pz IFF x < y %%%%%%%%%%%%%
%%% IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-1-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_pos_lt1.').
input(nl-1-pvs, [not(pz>0), not(x<y), x*pz<y*pz]). 
expected_output(nl-1-pvs, aff(la), kbo, [true]).    
expected_output(nl-1-pvs, aff(eq_la), kbo, [true]).    

%%% ONLY-IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-1-bis-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_pos_lt1.').
input(nl-1-bis-pvs, [not(pz>0), not(x*pz<y*pz), x<y]). 
expected_output(nl-1-bis-pvs, aff(la), kbo, [true]).    
expected_output(nl-1-bis-pvs, aff(eq_la), kbo, [true]).    

%%% both_sides_times_pos_lt2: LEMMA pz * x < pz * y IFF x < y %%%%%%%%%%%%%
%%% IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-2-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_pos_lt2.').
input(nl-2-pvs, [not(pz>0), not(x<y), pz*x<pz*y]). 
expected_output(nl-2-pvs, aff(la), kbo, [true]).    
expected_output(nl-2-pvs, aff(eq_la), kbo, [true]).    

%%% ONLY-IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-2-bis-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_pos_lt2.').
input(nl-2-bis-pvs, [not(pz>0), not(pz*x<pz*y), x<y]). 
expected_output(nl-2-bis-pvs, aff(la), kbo, [true]).    
expected_output(nl-2-bis-pvs, aff(eq_la), kbo, [true]).    

%%%  both_sides_times_neg_lt1: LEMMA x * nz < y * nz IFF y < x
%%% IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-3-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_neg_lt1.').
input(nl-3-pvs, [not(nz<0), not(y<x), x*nz<y*nz]). 
expected_output(nl-3-pvs, aff(la), kbo, [true]).    
expected_output(nl-3-pvs, aff(eq_la), kbo, [true]).    

%%% ONLY-IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-3-bis-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_neg_lt1.').
input(nl-3-bis-pvs, [not(nz<0), not(x*nz<y*nz), y<x]). 
expected_output(nl-3-bis-pvs, aff(la), kbo, [true]).    
expected_output(nl-3-bis-pvs, aff(eq_la), kbo, [true]).    

%%% both_sides_times_neg_lt2: LEMMA nz * x < nz * y IFF y < x %%%%%%%%%%%%
%%% IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-4-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_neg_lt2.').
input(nl-4-pvs, [not(nz<0), not(y<x), nz*x<nz*y]). 
expected_output(nl-4-pvs, aff(la), kbo, [true]).    
expected_output(nl-4-pvs, aff(eq_la), kbo, [true]).    

%%% ONLY-IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-4-bis-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_neg_lt1.').
input(nl-4-bis-pvs, [not(nz<0), not(nz*x<nz*y), y<x]). 
expected_output(nl-4-bis-pvs, aff(la), kbo, [true]).    
expected_output(nl-4-bis-pvs, aff(eq_la), kbo, [true]).    

%%% both_sides_times_pos_le1: LEMMA x * pz <= y * pz IFF x <= y
%%% IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-8-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_pos_le1.').
input(nl-8-pvs, [not(pz > 0), not(x =< y), x * pz =< y * pz]). 
expected_output(nl-8-pvs, aff(la), kbo, [true]).    
expected_output(nl-8-pvs, aff(eq_la), kbo, [true]).    

%%% ONLY-IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-8-bis-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_pos_le1.').
input(nl-8-bis-pvs, [not(pz > 0), not(x * pz =< y * pz), x =< y]). 
expected_output(nl-8-bis-pvs, aff(la), kbo, [true]).    
expected_output(nl-8-bis-pvs, aff(eq_la), kbo, [true]).    

%%% both_sides_times_pos_le2: LEMMA pz * x <= pz * y IFF x <= y
%%% IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-9-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_pos_le2.').
input(nl-9-pvs, [not(pz > 0), not(x =< y), pz * x =< pz * y]). 
expected_output(nl-9-pvs, aff(la), kbo, [true]).    
expected_output(nl-9-pvs, aff(eq_la), kbo, [true]).    

%%% ONLY-IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-9-bis-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_pos_le2.').
input(nl-9-bis-pvs, [not(pz > 0), not(pz * x =< pz * y), x =< y]). 
expected_output(nl-9-bis-pvs, aff(la), kbo, [true]).    
expected_output(nl-9-bis-pvs, aff(eq_la), kbo, [true]).    

%%% both_sides_times_neg_le1: LEMMA x * nz <= y * nz IFF y <= x
%%% IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-10-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_neg_le1.').
input(nl-10-pvs, [not(nz < 0), not(y =< x), x * nz =< y * nz]). 
expected_output(nl-10-pvs, aff(la), kbo, [true]).    
expected_output(nl-10-pvs, aff(eq_la), kbo, [true]).    

%%% ONLY-IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-10-bis-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_neg_le1.').
input(nl-10-bis-pvs, [not(nz < 0), not(x * nz =< y * nz), y =< x]). 
expected_output(nl-10-bis-pvs, aff(la), kbo, [true]).    
expected_output(nl-10-bis-pvs, aff(eq_la), kbo, [true]).    

%%% both_sides_times_neg_le2: LEMMA nz * x <= nz * y IFF y <= x
%%% IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-11-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_neg_le2.').
input(nl-11-pvs, [not(nz < 0), not(y =< x), nz * x =< nz * y]). 
expected_output(nl-11-pvs, aff(la), kbo, [true]).    
expected_output(nl-11-pvs, aff(eq_la), kbo, [true]).    

%%% ONLY-IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-11-bis-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times_neg_le2.').
input(nl-11-bis-pvs, [not(nz < 0), not(nz * x =< nz * y), y =< x]). 
expected_output(nl-11-bis-pvs, aff(la), kbo, [true]).    
expected_output(nl-11-bis-pvs, aff(eq_la), kbo, [true]).    

%%% both_sides_times1: LEMMA (x * n0z = y * n0z) IFF x = y %%%%%%%%%%%%%%%%
%%% IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-12-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times1.').
input(nl-12-pvs, [not(n0z = 0), not(x=y), x * n0z = y * n0z]). 
expected_output(nl-12-pvs, aff(la), prolog, [true]).    
expected_output(nl-12-pvs, aff(eq_la), prolog, [true]).    

%%% ONLY-IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-12-bis-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times1.').
input(nl-12-bis-pvs, [not(n0z = 0), not(x * n0z = y * n0z), x=y]). 
expected_output(nl-12-bis-pvs, aff(la), prolog, [true]).    
expected_output(nl-12-bis-pvs, aff(eq_la), kbo, [true]).    

%%% both_sides_times2: LEMMA (n0z * x = n0z * y) IFF x = y
%%% IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-13-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times2.').
input(nl-13-pvs, [not(n0z = 0), not(x=y), n0z * x = n0z * y]). 
expected_output(nl-13-pvs, aff(la), prolog, [true]).    
expected_output(nl-13-pvs, aff(eq_la), kbo, [true]).    

%%% ONLY-IF part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-13-bis-pvs, 
'Silvio Ranise', 
'This is extracted from the PVS prelude available at
          http://pvs.csl.sri.com/pvs/libraries/prelude.pvs 
 It is named both_sides_times2.').
input(nl-13-bis-pvs, [not(n0z = 0), not(n0z * x = n0z * y), x=y]). 
expected_output(nl-13-bis-pvs, aff(la), prolog, [true]).    
expected_output(nl-13-bis-pvs, aff(eq_la), kbo, [true]).    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Examples in the proof of termination of recursive functions %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-1-termination,
'Silvio Ranise',
'This is an example taken from the paper "Integrating Decision
 Procedures in Heuristic Theorem Provers: ..." by Boyer and
 Moore, page 102.  This problem differs from bm102 in that the lemma
 about multiplication is not provided here to the augmentation
 process.  Only the lemma encoding the positivity of ms is provided.
 Here, multiplication is handled by affinization.  Augmentation is
 important to relieve the hypotheses of lemmas generated by affinization.
 Finally, notice that there is no need to associate the nested
 multiplications to the right as in problem bm102.').
fact(nl-1-termination, ms_fact, [],0<ms(_)).
input(nl-1-termination, 
  	      [ms(c)+ms(d)*ms(d)+ms(b)*ms(b)<
                     ms(c)+ms(b)*ms(b)+2*(ms(d)*(ms(d)*ms(b)))+
                      ms(d)*(ms(d)*(ms(d)*ms(d)))]).
expected_output(nl-1-termination, aug_aff(la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
description(nl-2-termination, 
'Silvio Ranise', 
'This is taken from an example in the paper "Proving Termination of
 Normalization Functions for Conditional Expressions" by L C
 Paulson.  The details of how RDL solves this probelm can be found in
 "A Practical Extension Mechanism for Decision Procedures: the Case
  Study of Universal Presburger Arithmetic" by A. Armando and S. Ranise.'). 
fact(nl-2-termination, ms_fact, [],0<ms(_)).
input(nl-2-termination, 
                [ms(u)+ms(u)*ms(v)+ms(u)*ms(v)*ms(y)+ms(u)*ms(v)*ms(z)+ 
                 ms(u)*ms(w)+ms(u)*ms(w)*ms(y)+ms(u)*ms(w)*ms(z) < 
	          ms(u)+ms(u)*ms(v)+ms(u)*ms(w)+ 
	           ms(y)*ms(u)+ms(y)*ms(u)*ms(v)+ms(y)*ms(u)*ms(w)+
	           ms(z)*ms(u)+ms(z)*ms(u)*ms(v)+ms(z)*ms(u)*ms(w)]).
expected_output(nl-2-termination, aug_aff(la), kbo, [true]).    
expected_output(nl-2-termination, aug_aff(eq_la), kbo, [true]).    