%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  haness for the validation
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

:- ensure_loaded(utils_so).

%%%%%%%%%%%%%%%%%%
%
%  load the database files.
%

:- ensure_loaded(db).

%%%%%%%%%%%%%
%
%   load the test files.
%



:- ensure_loaded(sec74).     % Directives
:- ensure_loaded(sec78).     % Control Constructs
:- ensure_loaded(sec82).     % Unification
:- ensure_loaded(sec83).     % Types
:- ensure_loaded(sec84).     %   Term comparison
:- ensure_loaded(sec85).     % Term creation and decomposition
:- ensure_loaded(sec86).     %   Arithmetic evaluation
:- ensure_loaded(sec87).     %   Arihmentic comparison
:- ensure_loaded(sec88).     %  Clause retrieval and Information
:- ensure_loaded(sec89).     %  Clause creation and destruction
:- ensure_loaded(sec810).    %  All solutions
:- ensure_loaded(sec811).    % Stream selection and control
:- ensure_loaded(sec812).    % Character IO.
:- ensure_loaded(sec813).    % Byte IO
:- ensure_loaded(sec814).    % Term IO
:- ensure_loaded(sec815).    % Logic and Control
:- ensure_loaded(sec816).    % Atomic term processing
:- ensure_loaded(sec91).     % Simple arithmentic
:- ensure_loaded(sec92).     % Contains tests that use the flag maxint.
:- ensure_loaded(sec93).     % Other arithmetic functors
:- ensure_loaded(sec94).     % bitwise functors
:- ensure_loaded(sec817).

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

	
