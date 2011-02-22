%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   Section 8.3 of the ISO Prolog Standard     %
%                                              %
%   Type testing.
%                                              %
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%   Thanks to Ken Bowen of ALS for support     %
%   and to Joe Pedano and John Hallat of       %
%   Saint Joseph's for their work on this      %
%   project.                                   %
%                                              %
%   Version for calypso   2 oct 1998           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


test_types :-
	test_false(var(foo)),
	test_true(var(Foo)),
	test_false((foo=Bar, var(Bar))),
        test_true(var(_)),
	test_true(atom(atom)),
	test_true(atom('string')),
        test_false(atom(a(b))),
        test_false(atom(Var)),
        test_true(atom([])),
        test_false(atom(6)),
        test_false(atom(3.3)),
	test_true(integer(6)),
	test_true(integer(-3)),
	test_false(integer(3.3)),
	test_false(integer(X)),
	test_false(integer(atom)),
        test_true(float(3.3)),
        test_true(float(-3.3)),
        test_false(float(atom)),
        test_false(float(X1)),
	test_true(atomic(atom)),
        test_false(atomic(a(b))),
        test_false(atomic(V1)),
        test_true(atomic(2.3)),
        test_false(compound(33.3)),
        test_false(compound(-33.3)),
        test_false(compound(_)),
        test_false(compound(a)),
        test_false(compound([])),
        test_true(compound(a(b))),
	test_true(compound([a])),
        test_true(nonvar(33.3)),
	test_true(nonvar(foo)),
	test_true((foo= Baz, nonvar(Baz))),
        test_false(nonvar(FOO)),
	test_false(nonvar(_)),
	test_true(nonvar(a(b))),
        test_true(number(3)),
	test_true(number(3.3)),
	test_true(number(-3)),
	test_false(number(a)),
	test_false(number(X)).
       
    
test_83:-  

	log( 'Starting tests for Section 8.3'), log_nl,
	log_nl, log( 'Testing Type testing predicates.'), log_nl,

	test_types,
	
	log_nl, log('Tests of Type testing predicates complete.'), 
	

	log_nl, log('All testing completed for Section 8.3.'), 
        log_nl, log_nl, !.
