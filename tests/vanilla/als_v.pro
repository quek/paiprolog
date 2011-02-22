%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Harness for the validation
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
%
:-  [utils_so].

%%%%%%%%%%%%%%%%%%
%
%  load the database files.
%

:-  [db].

%%%%%%%%%%%%%
%
%   load the test files.
%


% each of these contains a clause for the multifile
%

%:-[sec74].
:-[sec78].
:-[sec82].
:- [sec83].  
:- [sec84].
:-[sec85].
:- [sec86].
:- [sec87].
:-  [sec88].
:-  [sec89].
:-  [sec810].
:-  [sec811].
:-  [sec812].
:-  [sec813].
:-  [sec814].
:-  [sec815].
:-  [sec816].

:-  [sec91].
:-  [sec92].   
:-  [sec93].
:-  [sec94].
:-  [sec817].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  run the tests
%
%

run_tests:-  
% test_74,
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

	
