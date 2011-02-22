%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%    sec94.pl                                  %
%                        12:40PM  5/4/1996     %
%                                              %
%  tests of arithmetic functors only test for  %
%  support of bitwise functors.                %
%  (/\)/2, (\/)/2,  (\)/1, <</2, >>/2          %
%                                              % 
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%                                              %
%                                              %
%   May be used freely provided                %
%   acknowledgement is made.                   %
%                                              %
%   Thanks to Ken Bowen of ALS for support     %
%   and to Joe Pedano and John Hallat of       %
%   Saint Joseph's for their work on this      %
%   project.                                   %
%                                              %
%   Version for calypso   1 oct 1998           %
%                                              %
%   Modified to correspond to what the         %
%   standard should say                        %
%   8 october 1998                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    test the  and function
%

test_and :-
   eval_or_fail(_ is 1 /\ 2),
   test_true( X1 is 15 /\ 12),
   test_val(X2 is 10 /\ 12,X2,8),
   error_test(X3 is 3 /\ N ,instantiation_error),
   error_test(X3a is 3.5 /\ 2, type_error(integer, 3.5)),
   error_test(X4 is foo/\2 ,type_error(evaluable, foo/0)).
   

test_and :-
	log_nl, log( 'bitwise and (/\\)/2 not supported.'),
         log_nl, log_nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  test the or function
%

test_or :-
   eval_or_fail(_ is 1 \/ 1),
   test_true( X1 is 10  \/ 12),
   test_val(X2 is 10 \/  12,X2,14),
   error_test(X3 is 3 \/ N ,instantiation_error),
   error_test(X4 is 5.6\/2 ,type_error(integer, 5.6)),
   error_test(X5 is foo\/2 ,type_error(evaluable, foo/0)).
   

test_or :-
         log_nl, log( 'bitwise or (\\/)/2 not supported.'),
         log_nl, log_nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Test of ones complement
%

test_ones_complement :- 
        eval_or_fail(_ is \1),
	test_true(X1 is \10),
        test_val(X2 is \(\10), X2, 10),
        error_test(X3 is \ N ,instantiation_error),
         error_test(X4 is \(3.14) ,type_error(integer, 3.14)),
         error_test(X4 is \foo ,type_error(evaluable, foo/0)).
   
test_ones_complement :-
         log_nl, log( 'bitwise complement  \\/2 not supported.'),
         log_nl, log_nl.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  tests of shifts.
%

test_shifts :-
     eval_or_fail(_ is 56  << 1),
     test_true(X1 is 2 << 3),
     test_val(X2  is  16 << 2, X2, 64),
     test_true(X3 is 32 >> 1),
     test_val(X4 is 19 >> 2, X4, 4),
     error_test(X3 is 3 << N ,instantiation_error),
     error_test(X4 is 3<<foo ,type_error(evaluable, foo/0)),
     error_test(X3 is N << 4 ,instantiation_error),
     error_test(X4 is foo<< 3 ,type_error(evaluable, foo/0)),
     error_test(X4 is (6.7)<< 3 ,type_error(integer, 6.7)),
     error_test(X4 is 6<< (3.4) ,type_error(integer, 3.4)),
     error_test(X3 is 3 >> N ,instantiation_error),
     error_test(X4 is 3>>foo ,type_error(evaluable, foo/0)).


test_shifts:-
	log_nl, log_nl,
        log('Shifts not supported'), log_nl, log_nl.
test_94 :-
   log_nl, log('testing bitwise  arithmetic functors'),
   log_nl, log_nl,
   log('testing and or and 1s complement '),
   log_nl,
   test_and,
   test_or,
   test_ones_complement,
   log_nl, log('Done testing and or and 1s complement'), 
   log_nl, log('testing shift functions'),
   log_nl,
   test_shifts,
   log_nl, log('Done testing shifts'),
   log_nl,
   log('Done testing section 9.4'),
   log_nl.
