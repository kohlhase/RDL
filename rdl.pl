:- ensure_loaded(library(lists)).
:- ensure_loaded(problem).

%%% The following list of predicates%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% consistitues (part of) the RDL user-interface for %%%%%%%%%%
%%% specifying problems %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic description/3,
	   simbol_weight/4,
	   ord_gt/4,
	   pred_sym/2,
	   fact/4,
	   input/2,
	   expected_output/4.

%%% Technical description of the system %%%%%%%%%%%%%%%%%%%%%%%%%
rdl_release('1.1').
rdl_banner(X) :- 
	rdl_release(V),
	atom_concat('               RDL v. ',V,P),
	atom_concat('\n-----------------------------------------\n',P,P1),
	atom_concat(P1,'\n-----------------------------------------\n',X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Predefined predicate symbols: pred_sym/2 is a dynamic 
%%% predicate
:- assert(pred_sym(_,_=_)).
:- assert(pred_sym(_,_=<_)).
:- assert(pred_sym(_,_<_)).
:- assert(pred_sym(_,_>_)).
:- assert(pred_sym(_,_>=_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% To load the RDL input file %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_problems_file(F) :-
	unload_problems_database,
	open(F,read,STR),
	assert_read_terms(STR),
	close(STR).

% Unload the facts about RDL problems from the SICStus database 
unload_problems_database :-
	retractall(description(_,_,_)),
	retractall(input(_,_)),
	retractall(fact(_,_,_,_)),
	retractall(expected_output(_,_,_,_)),
	retractall(pred_sym(_,_)),
	retractall(symbol_weight(_,_,_,_)),
	retractall(ord_gt(_,_,_,_)),
	assert(pred_sym(_,_=_)),
	assert(pred_sym(_,_=<_)),
	assert(pred_sym(_,_<_)),
	assert(pred_sym(_,_>_)),
	assert(pred_sym(_,_>=_)).

% Assert every terms of the input file %%%%%%%%%%%%%%%%%%%%%%%%%% 
assert_read_terms(STR) :-
	read(STR,T),
	(T\==end_of_file ->
	    assert(T),
	    assert_read_terms(STR)
	;
	    true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run :- run(_, _, _, _), fail.
run(Ver) :- run(_, Ver, _, _), fail.
run(Ver,Problem) :- run(Problem, Ver, _, _).
run(Problem, Version, Order) :-
	run(Problem, Version, Order, _).

%%% Running the system %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run(Problem, Version, Order, Status) :-
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	description(Problem, _, _),   % The problem to be considered.
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        input(Problem, InPhi),        % Formula to simplify
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        expected_output(Problem, Version, Order, OutPhi),
	format('Problem: ~w\n', [Problem]),
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(Version =..[_,DP];Version =..[DP]),
	ensure_loaded(DP),
        %%% FIRST output phase %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	describe_dp(Version, VersionDescription),
	describe_ord(Order, OrderVersion),
 	format('Reasoning Specialist: ~w\n', [VersionDescription]),
	format('Ordering: ~w\n',            [OrderVersion]),
	format('Input Formula: \t\t~w\n', [InPhi]),
	format('Expected Formula: \t~w\n', [OutPhi]), 
        %%% END of FIRST output phase %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        statistics(runtime, [_, LoadT]),
	%%% START of Simplification activity %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	reduce_for_debugging(Version-Problem-Order,
	reduce(Version-Problem-Order,
	       seq(simp, InPhi, SimpPhi, []),
	       M),
	%%% END of Simplification activity %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	statistics(runtime, [_, ExecT]),
        %%% Cleaning-up the resulting reduction%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	simp_red(M, SM),
        %%% SECOND output phase %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	format('Simplified Formula: \t~w\n', [SimpPhi]),
	( SimpPhi==OutPhi ->
           Status=ok, format('Status: ok!\n',[]) ;
	   Status=fail, format('Status: failed!\n',[]) ),
	format('Reduction: ~w\n', [SM]),
	statistics(runtime, [_, OutT]),
	TotT is LoadT+ExecT+OutT,
	format('Time (Elapsed-Theorem Proving): ~w-~w msec\n\n', [TotT,ExecT]).
        %%% END of SECOND output phase %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Cleanining-up reductions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simp_red([],[]) :- !.
simp_red(S1:[],SS) :- !,
	simp_red(S1,SS).
simp_red(S1>id,SS) :- !,
	simp_red(S1,SS).
simp_red(S1:[id],SS) :- !,
	simp_red(S1,SS).
simp_red(S1:S2,SS1:SS2) :- !,
	simp_red(S1,SS1),
	simp_red(S2,SS2).
simp_red(S1>S2,SS1>SS2) :- !,
	simp_red(S1,SS1),
	simp_red(S2,SS2).
simp_red([S|Ss],[SS|SSs]) :- !,
	simp_red(S,SS),
	simp_red(Ss,SSs).
simp_red(S,S1) :-
	S=..[F|Ss],
	simp_red(Ss,Ss1),
	S1=..[F|Ss1].
