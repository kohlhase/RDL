%%% ACCESSING AND DISPLAYING THE DATA BASE OF PROBLEMS.

% ordering(Name,    % Name of the problem
%          Version, % Instance of the system
%          Order,   % Ordering to be used.
ordering(Ex, Ver, Ord) :- expected_output(Ex, Ver, Ord, _). % NON DOVREBBE SERVIRE PIU'

% Looking for rewrite rules that we can use in both directions.
rrule(_-Ex, Q, Lhs, Rhs) :- 
	fact(Ex, _, Q, Lhs=Rhs) ; 
	fact(Ex, _, Q, Rhs=Lhs).

% Looking for constraint rules.
crule(_-Ex, Q, C) :- fact(Ex, _, Q, C).

%%% These predicates serve for querying the data base %%%%%%%%%%%%%%
%%% of problmes which RDL knows about. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% display_description/1: given the tag uniquely identifing a problem,
%                        this predicate displays the name of the
%                        author and a description.
% 
% display_fact/2,
% display_facts/1: given the tag uniquely identifing a problem and
%                  (for the first predicate) the name of the fact
%                  display the fact or all the facts for the problem. 
%
% display_in_formula/1: given the tag uniquely identifing a problem
%                       display the formula to be simplified
% display_expected_output/1: given the tag uniquely identifing a
%                            problem display the formula to get from
%                            the simplification process and the
%                            settings to obtain it.
% 
% display_settings/1: given the tag uniquely identifing a
%                     problem display the settings used to get a
%                     certain formula as output 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Displaying the available information about a 
% problem given its tag.  
display_description(Problem) :- 
	description(Problem, Author, Description),
	format('Problem: ~w\n',[Problem]),
	format('Author: ~w\n', [Author]),
	format('Description: ~w\n', [Description]).

display_fact(Problem, Name) :- 
	format('Problem: ~w\n', [Problem]), 
	fact(Problem, Name, Prems, Concl),
	format('Fact: (~w)', [Name]),
	format(' ~w --> ~w\n', [Prems, Concl]).

display_facts(Problem) :- display_fact(Problem, _), fail.
%%% End of Facts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_in_formula(Problem) :- 
	input(Problem, Formula),
	format('Input Formula: ~w\n', [Formula]).
	
display_expected_output(Problem) :-
	expected_output(Problem, Version, Order, Formula),
	describe_dp(Version, VersionDescription),
	describe_ord(Order, OrderVersion),
	format('Expected formula from simplification: ~w\n', [Formula]), 
	format('Decision procedure: ~w\n', [VersionDescription]),
	format('ordering: ~w\n',            [OrderVersion]).
%%% End of Input & Output formulae %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Settings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display_settings(Problem) :-
	expected_output(Problem, Version, Order, _),
	describe_dp(Version, VersionDescription),
	format('\nDecision procedure for the ~w\n', [VersionDescription]),
	describe_ord(Order, OrderDescription),
	format('Ordering: ~w\n', [OrderDescription]).

% describe_dp(Version, DecProcId)
% Modified for new language of rdl
describe_dp(eq,
	    'theory of ground equality.').
describe_dp(aug(eq),
	    'theory of ground equality with augmentation enabled.').
describe_dp(la,
	    'linear arithmetic.').
describe_dp(aff(la),
	    'linear arithmetic with affinization enabled.').
describe_dp(aug_aff(la),
	    'linear arithmetic with a combination of augmentation and affinization enabled.').
describe_dp(aug(la),
	    'linear arithmetic with augmentation enabled.').
describe_dp(eq_la,
	    'combination of the theory of ground equality and Linear Arithmetic.').
describe_dp(aff(eq_la),
	    'combination of the theory of ground equality and Linear Arithmetic with affinization enabled.').
describe_dp(aug(eq_la),
	    'combination of the theory of ground equality and Linear Arithmetic with augmentation enabled.').
describe_dp(aug_aff(eq_la),
	    'combination of the theory of ground equality and Linear Arithmetic with a combination of augmentation and affinization enabled.').

% describe_ord(Order, OrderDescription),
describe_ord(prolog, 'Standard.').
describe_ord(kbo, 'Knuth-Bendix.').
describe_ord(none,   'None.').

% describe_order(Order, OrderDescription),
describe_order(prolog, 'Prolog standard total ordering over terms.').
describe_order(none,   'None.').
%%% End of Settings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

