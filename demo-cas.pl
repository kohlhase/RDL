%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% RDL Examples %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	 
% load_problems_file('demo-cas.pl').

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(eq,zhang).
description(zhang,
'Alessandro Armando',
'The following example comes from H. Zhang paper on Contextual Rewriting.').
fact(zhang, rem_times, [not(U=0)],rem(U*_,U)=0).
input(zhang, [rem(y*z,x)=0,not(x*y=y*z),x=0]).
ord_gt(zhang,_,y,x).
expected_output(zhang, eq, kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(la,zhang_la).
description(zhang_la,
'Alessandro Armando',
'This comes from H. Zhang paper on Contextual Rewriting.').
fact(zhang_la, rem_rew, [U>0],rem(U*_,U)=0).
input(zhang_la, [rem(x*y,x)=0,x=<0]).
expected_output(zhang_la, la, kbo, [true]).
	
%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(aug(la),bm94).
description(bm94,
'Alessandro Armando',
'This is an example taken from the paper "Integrating Decision
Procedures in Heuristic Theorem Provers: ..." by Boyer and
Moore, page 94.  See bm94_simplified for a simplified version
which does not need augmentation.').
fact(bm94, min_lesseq_max, [], min(L)=<max(L)).
input(bm94, [not(k>=0), not(l>=0), not(l=<min(b)), not(0<k), l<max(b)+k]).
expected_output(bm94, aug(la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(aug(eq_la),kapur_nie_11).
description(kapur_nie_11,
'Alessandro Armando',
'Kapur example at pag. 11 of the paper "Reasoning about numbers in Tecton".').
pred_sym(kapur_nie_11,p(_)).
fact(kapur_nie_11, min_rew, [max(X,Y)=X], min(X,Y)=Y).
fact(kapur_nie_11, f_lesseq_g, [p(X)], f(X)=<g(X)).
input(kapur_nie_11, [not(p(x)), not(z=<f(max(x,y))), not(0<min(x,y)),
                     not(x=<max(x,y)), not(max(x,y)=<x), z<g(x)+y]).
expected_output(kapur_nie_11, aug(eq_la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(aug(la),bm102).
description(bm102,
'Alessandro Armando',
'This is an example ...').
fact(bm102, arith_fact, [0<I],J=<I*J).
fact(bm102, ms_fact, [],0<ms(_)).
input(bm102, [ms(c)+ms(d)*ms(d)+ms(b)*ms(b)<
               ms(c)+ms(b)*ms(b)+2*(ms(d)*ms(d))*ms(b)+
                ms(d)*(ms(d)*(ms(d)*ms(d)))]).
expected_output(bm102, aug(la), kbo, [true]).

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(aff(la),nl-example-bjorner).
description(nl-example-bjorner, 
'Silvio Ranise', 
'This is taken from N. Bjorner PhD Thesis (Example at page 76).').
input(nl-example-bjorner, 
	[not(x>0), not(y>0), not(z>0), x*y*z+x*y*z*z>0]).
expected_output(nl-example-bjorner, aff(la), kbo, [true]).    

%%% Example %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(aug_aff(la),nl-1-termination).
description(nl-1-termination,
'Silvio Ranise',
'This is an example ...').
fact(nl-1-termination, ms_fact, [],0<ms(_)).
input(nl-1-termination, 
  	      [ms(c)+ms(d)*ms(d)+ms(b)*ms(b)<
                     ms(c)+ms(b)*ms(b)+2*(ms(d)*(ms(d)*ms(b)))+
                      ms(d)*(ms(d)*(ms(d)*ms(d)))]).
expected_output(nl-1-termination, aug_aff(la), kbo, [true]).    
