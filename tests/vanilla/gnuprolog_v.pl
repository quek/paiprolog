
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  harness for the validation
%  suite for calypso
%
%  Jonathan Hodgson.
%
%   2 October 1998.
%


%%%%%%%%%%
%
%   load the utilities.
%

:- initialization([utils_so]).

%%%%%%%%%%%%%%%%%%
%
%  load the database files.
%

:- initialization([db]).

%%%%%%%%%%%%%
%
%   load the test files.
%



:- initialization([sec74]).
:- initialization([sec78]).
:- initialization([sec82]).
:- initialization([sec83]). 
:- initialization([sec84]).
:- initialization([sec85]).
:- initialization([sec86]).
:- initialization([sec87]).
:- initialization([sec88]).
:- initialization([sec89]).
:- initialization([sec810]).
:- initialization([sec811]).
:- initialization([sec812]).
:- initialization([sec813]).
:- initialization([sec814]).
:- initialization([sec815]).
:- initialization([sec816]).

:- initialization([sec91]).
:- initialization([sec92]).   % Contains tests that use the flag maxint.
:- initialization([sec93]).
:- initialization([sec94]).
:- initialization([sec817]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  run the tests
%
%

run_tests:-  
        test_74,
        test_78,
        test_82,
        test_83,  
        test_84,
        test_85,
        test_86,
        test_87,
        test_88,
        test_89,
        test_810,
        test_811,
        test_812,
        test_813,
        test_814,
        test_815,
        test_816,
        test_91,
        test_92,		
        test_93,
        test_94,
         test_817.

validate:-
   start_log,
   run_tests,
   end_log.
validate.

	
