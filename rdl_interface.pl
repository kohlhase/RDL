%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% rdl_interface.pl: contains the interface predicate of rdl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Luca Compagna                    
%%%         (E-mail: compa@mrg.dist.unige.it)            
%%%                University of Genova                   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This line must be commented when you use the   
%%% predicate make_rdl_executable/[0,1] contained 
%%% in the prolog file rdl_management.pl
%%% Otherwise you must copy the file rdl.pl into
%%% the destination directory
% :- ensure_loaded(rdl).

:- ensure_loaded(library(lists)).

runtime_entry(start):-
    prolog_flag(argv,ARGS),
    ((member('--help',ARGS);member('-h',ARGS)) ->
	format('Usage:\n',[]),
	format('~2|rdl OPTION~n',[]),
	format('~2|rdl FILE [OPTIONS]~n~n',[]),
	format('OPTION:~n',[]),
	format('~2|-h,~7|--help~20|display this help and exit~n',[]),
	format('~2|-v,~7|--version~20|output version information and exit~n',[]),
	format('~2|-ex,~7|--examples~20|output some examples of usage of the RDL executable~n~n',[]),
	format('OPTIONS:~n',[]),	
 	format('~2|-ord=ORD,~12|--ordering=ORD~29|ORD is the ordering used.~n',[]),
 	format('~29|Currently accepted values:~n',[]),
 	format('~31|\'prolog\'~39|: ',[]), 
	print_string_in_cell(41,'lexicographics prolog ordering is used',78,41),
	nl,
 	format('~31|\'none\'~39|: ',[]),
	print_string_in_cell(41,'no ordering is used',78,41),
	nl,
 	format('~2|-pb=PB,~13|--problem=PB~29|PB is the problem\'s name~n',[]),
 	format('~2|-rs=RS~29|RS is the reasoning specialist used.~n',[]),
 	format('~29|Currently accepted values:~n',[]),
	TAB1 = 50,
	TAB2 = 78,
 	format('~31|\'eq\'~48|: ',[]),
	print_string_in_cell(TAB1,'theory of ground equality',TAB2,TAB1),
	nl,
 	format('~31|\'la\'~48|: ',[]),
	print_string_in_cell(TAB1,'linear arithmetic over integers',TAB2,TAB1),
	nl,
 	format('~31|\'eq_la\'~48|: ',[]),
	print_string_in_cell(TAB1,'combination of the theory of ground equality and linear arithmetic over integers',TAB2,TAB1),
	nl,
 	format('~31|\'aug(eq)\'~48|: ',[]),
	print_string_in_cell(TAB1,'theory of ground equality with augmentation enabled',TAB2,TAB1),
	nl,
 	format('~31|\'aug(la)\'~48|: ',[]),
	print_string_in_cell(TAB1,'linear arithmetic over integers with augmentation enabled',TAB2,TAB1),
	nl,
 	format('~31|\'aug(eq_la)\'~48|: ',[]),
	print_string_in_cell(TAB1,'combination of the theory of ground equalityand linear arithmetic over integers with augmentation enabled',TAB2,TAB1),
	nl,
 	format('~31|\'aff(la)\'~48|: ',[]),
	print_string_in_cell(TAB1,'linear arithmetic over integers with affinization enabled',TAB2,TAB1),
	nl,
 	format('~31|\'aff(eq_la)\'~48|: ',[]),
	print_string_in_cell(TAB1,'combination of the theory of ground equalityand linear arithmetic over integers with affinization enabled',TAB2,TAB1),
	nl,
 	format('~31|\'aug_aff(la)\'~48|: ',[]),
	print_string_in_cell(TAB1,'linear arithmetic over integers with a combination of augmentation and affinization enabled',TAB2,TAB1),
	nl,
 	format('~31|\'aug_aff(eq_la)\'~48|: ',[]),
	print_string_in_cell(TAB1,'combination of the theory of ground equality and linear arithmetic over integers with a combination of augmentation and affinization enabled',TAB2,TAB1),
	nl,
	nl,
	print_string_in_cell(0,'FILE is the file containing a data base of problems. (See the RDL user manual for the format)',78,0),
	nl,
	nl,
	halt
    ;
	true
    ),
    ((member('--version',ARGS);member('-v',ARGS)) ->
	rdl_release(Ver),
	nl,
	format("RDL v. ~p\n",[Ver]),
	print_string_in_cell(0,'Copyright (C) 1995-1998 by DIST',78,0),
	nl,
	print_string_in_cell(0,'Universita\' degli Studi di Genova.',78,0),
	nl,
	print_string_in_cell(0,'e-mail: rdl-support@mrg.dist.unige.it',78,0),
	nl,
	nl,
	print_string_in_cell(0,'This system is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file LICENSE and the GNU Library General Public License for more details.',78,0),
	nl,nl,
	halt
    ;
	true
    ),
    ((member('--examples',ARGS);member('-ex',ARGS)) ->
	format("Examples of RDL command lines:\n\n",[]),
	format("Ex.1~5|: ",[]),
	print_string_in_cell(7,'to run all the problems available in the file \'problems.pl\' you can issue:',78,7),
	nl,
	nl,
	format("~7|rdl problems.pl\n\n",[]),
	format("Ex.2~5|: ",[]),
	print_string_in_cell(7,'to run the problems contained in the file \'problems.pl\' using the reasoning specialist for linear arithmetic over integers (\'la\') you can issue:',78,7),
	nl,nl,
	format("~7|rdl -rs='la' problems.pl\n\n",[]),
	format("Ex.3~5|: ",[]),
	print_string_in_cell(7,'to run the problems contained in the file \'problems.pl\' using the reasoning specialist for linear arithmetic with augmentation enabled and the prolog lexicographic ordering (\'prolog\') you can issue:',78,7),
	nl,nl,
	format("~7|rdl -ord='prolog' -rs='aug(la)' problems.pl\n\n",[]),
	format("Ex.4~5|: ",[]),
	print_string_in_cell(7,'to run the problem label contained in the file \'problems.pl\' characterized by the name \'zhang\' you can issue:',78,7),
	nl,nl,
	format("~7|rdl -pb='zhang' problems.pl\n\n",[]),
	halt
    ;
	true
    ),
    rdl_banner(Banner),
    write(Banner),
    (parse_args(ARGS,FI,Pb,RS,ORD)->
	true;
	write('ERROR: syntax error in command line.'),nl,
	write('       Please issue "rdl --help" to see the help.'),nl,
	nl,
	nl,
	halt
	),
    (\+var(FI) ->
	true
    ;
	write('ERROR: you must specify the input file'),nl,
	write('       (e.g. \" problems.pl \"). '),
	nl,
	nl,
	halt
    ),
%     (file_exists(FI) ->
% 	true
%     ;
% 	write('ERROR: the specified input file does not exist'),nl,
% 	nl,
% 	nl,
% 	halt
%     ),
    % Load input problems
%   [FI],
    load_problems_file(FI),
    % This predicate always fails
    \+run_cover(Pb,RS,ORD),
    halt.


% runtime_entry(start):-
%     prolog_flag(argv,ARGS),
%     ((member('--help',ARGS);member('-h',ARGS)) ->
% 	write('Usage: rdl OPTION'),nl,
% 	write('       rdl FILE'),
% 	nl,nl,
% 	write('OPTION:'),nl,	
% 	write('       -h, --help       display this help and exit'),nl,
% 	write('       -v, --version    output version information and exit'),nl,nl,
% 	write('FILE is the file containing a data base of problems. (See '),nl, 
% 	write('the RDL user manual for the format)'),
% 	nl,
% 	nl,
% 	halt
%     ;
% 	true
%     ),
%     ((member('--version',ARGS);member('-v',ARGS)) ->
% 	rdl_release(Ver),
% 	format("RDL v. ~p\n",[Ver]),
% 	format("Copyright (C) 1995-1998 by DIST\n",[]),
% 	format("Universita' degli Studi di Genova.\n",[]),
% 	format("This system is distributed in the hope \n",[]),
% 	format("that it will be useful, but WITHOUT ANY \n",[]),
% 	format("WARRANTY; without even the implied \n",[]),
% 	format("warranty of MERCHANTABILITY or FITNESS \n",[]),
% 	format("FOR A PARTICULAR PURPOSE.  See the file \n",[]),
% 	format("LICENSE and the GNU Library General \n",[]),
% 	format("Public License for more details.\n",[]),
% 	halt
%     ;
% 	true
%     ),
%     rdl_banner(Banner),
%     write(Banner),
%     nl,
%     (ARGS=[FI] ->
% 	true
%     ;
% 	write('ERROR: you must specify the input file'),nl,
% 	write('       (e.g. \" problems.pl \"). '),
% 	nl,
% 	nl,
% 	halt
%     ),
%     % Load input problems
%     [FI],
%     nl,
%     write('Write either: '),nl,
%     write('      "run(Pb,RS,Ord)." '),nl,
%     write('          RDL simplifies the problem Pb by means of the'),nl,
%     write('          reasoning specialist RS and the ordering Ord.'),nl,
% %     write('      where: Pb  is the problem name,'),nl,
% %     write('             RS  is the reasoning specialist (see user\'s manual),'),nl,
% %     write('             Ord is the ordering (see user\'s manual).'),nl,
% %     nl,
%     write('or '),nl,
%     write('      "run." '),nl,
%     write('          RDL simplifies all problems contained in the'),nl,
%     write('          input file.'),nl,nl,
%     read(Run),nl,
%     (Run=..[run,Pb,RS,Ord] ->
% 	\+run_cover(Pb,RS,Ord)
%     ;
% 	(Run=..[run] ->
% 	    \+call(run);
% 	    write('Syntax error.'),nl,
% 	    halt
% 	)
%     ),
%     halt.

run_cover(Pb,RS,Ord):-
	run(Pb,RS,Ord),fail.

parse_args([],_,_,_,_).
parse_args([ARG|ARGS],FI,Pb,RS,ORD):-
	atom_chars(ARG,CARG),
        parse_arg(pb,Pb,CARG,[]),
	parse_args(ARGS,FI,Pb,RS,ORD).
parse_args([ARG|ARGS],FI,Pb,RS,ORD):-
	atom_chars(ARG,CARG),
        parse_arg(rs,RS,CARG,[]),
	parse_args(ARGS,FI,Pb,RS,ORD).
parse_args([ARG|ARGS],FI,Pb,RS,ORD):-
	atom_chars(ARG,CARG),
        parse_arg(ord,ORD,CARG,[]),
	parse_args(ARGS,FI,Pb,RS,ORD).
parse_args([ARG|ARGS],FI,Pb,RS,ORD):-
	ARG=..[FI],
	atom_chars(ARG,CARG),
        \+parse_arg(pb,_,CARG,[]),
        \+parse_arg(rs,_,CARG,[]),
        \+parse_arg(ord,_,CARG,[]),
	parse_args(ARGS,FI,Pb,RS,ORD).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PARSING RULES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_arg(pb,Pb) -->
	"-pb=",
	parse_pb(Pb).
parse_arg(pb,Pb) -->
	"--problem=",
	parse_pb(Pb).

parse_arg(rs,RS) -->
	"-rs=",
	parse_rs(RS).

parse_arg(ord,none) -->
	"-ord=none".
parse_arg(ord,none) -->
	"--ordering=none".
parse_arg(ord,prolog) -->
	"-ord=prolog".
parse_arg(ord,prolog) -->
	"-ordering=prolog".

parse_pb(C) --> 
	parse_segments(Ss),
	{segments_union(Ss,C)}.

parse_rs(aug(DP)) --> 
	"aug(",
	parse_dp(DP),
	")".

parse_rs(aug_aff(DP)) --> 
	"aug_aff(",
	parse_dp(DP),
	")".
	
parse_rs(aff(DP)) --> 
	"aff(",
	parse_dp(DP),
	")".

parse_rs(DP) --> 
	parse_dp(DP).

parse_dp(eq) --> "eq".
parse_dp(la) --> "la".
parse_dp(eq_la) --> "eq_la".

parse_segments([]).
parse_segments([S]) --> 
	const(S).
parse_segments([S|Ss]) --> 
	const(S),
	"-", 
	parse_segments(Ss).

segments_union(Ss,C) :-
	reverse(Ss, Rs),
	segments_union_reverse(Rs,C).

segments_union_reverse([S],S).
segments_union_reverse([S|Ss],O-S) :-
	segments_union_reverse(Ss,O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LOW LEVEL GRAMMAR RULES

const(C) --> 
	const_code(CCod), 
	{atom_chars(C,CCod)}.

const(N) --> 
	number(NCod), 
	{number_chars(N,NCod)}.

const_code([C|Cs]) --> first_const_char(C), rest_const_code(Cs).

const_code([C]) --> first_const_char(C).

rest_const_code([C|Cs]) --> const_char(C), rest_const_code(Cs).

rest_const_code([C]) --> const_char(C).

first_const_char(C) --> [C],{"a"=<C, C=<"z"}.

const_char(C) --> [C],{"a"=<C, C=<"z"}.

const_char(C) --> [C],{"0"=<C, C=<"9"}.

const_char(D) --> [C], {C=:="_", D is 95}.

number([X]) --> num(X).

number([Y|Z]) --> num(Y),number(Z).

number([]) --> [].

num(C) --> [C], {num_char(C)}.

num_char(C) :- ("0"=<C, C=<"9"); C=:=".". 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRINT PREDICATES

% print_string_in_cell(BT,STR,ET,CPOS) :- 
%      This predicate print the string STR in the cell 
%      characterized by the left column boundary at 
%      line position BT and the right column boundary 
%      at line position ET. CPOS is the current position 
%      of the cursor.
print_string_in_cell(BT,STR,ET,CPOS) :-
	ET > BT,
	atom_chars(STR,CSTR),
	print_strcode_in_cell(BT,CSTR,ET,CPOS).

print_strcode_in_cell(_,[],_,_).
%	format("~n",[]).

print_strcode_in_cell(BC,CSTR,EC,CPOS) :-
	CSTR\==[],
	DC is EC - BC + 1,
	(CPOS - BC < 0 ->
	    print_spaces(BC - CPOS),
	    NC is 0;
	    NC is CPOS - BC
	),
	(\+member(32,CSTR) ->
	    CW = CSTR,
	    RCSTR = [];
	    append(CW,[32|RCSTR],CSTR)
	),
	same_length(CW,CW,LW),
	LW =< DC,
	CC is NC + LW,
	(CC =< DC -> 
	    format(CW,[]),
	    (RCSTR \== [] -> 
		format(" ",[])
	    ;
		true
	    ),
	    print_strcode_in_cell(BC,RCSTR,EC,BC+CC+1)
	;
	    format("~n",[]),
	    print_strcode_in_cell(BC,CSTR,EC,0)
	).	    

% WORD LONGER THAN THE DIMENSION OF THE CELL
print_strcode_in_cell(BC,CSTR,EC,_) :-
	CSTR\==[],
	DC is EC - BC + 1,
	(\+member(32,CSTR) ->
	    CW = CSTR,
	    RCSTR = [];
	    append(CW,[32|RCSTR],CSTR)
	),
	same_length(CW,CW,LW),
	LW > DC,
        format("~n",[]),
	print_spaces(BC),
        format(CW,[]),
	print_strcode_in_cell(BC,RCSTR,EC,BC + LW).

print_spaces(0).
print_spaces(NS) :-
	NS > 0,
	format(" ",[]),
	DECNS is NS-1, 
	print_spaces(DECNS).


