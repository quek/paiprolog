%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Database of predicates used by several
%  sections of the suite.
%
%    Jonathan Hodgson
%	2 October 1998.
%



%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%    Database for tests. Taken from 8.8 and    %
%    8.9 of the standard.                      %
%    the standard ISO/IEC 13211-1              %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(cat/0).
cat.

:- dynamic(dog/0).
dog :- true.

elk(X) :- moose(X).

:- dynamic(legs/2).


legs(A, 6) :- insect(A).
legs(A, 7) :- A, call(A).
:- dynamic(insect/1).
insect(ant).
insect(bee).

:- dynamic(foo/1).
foo(X) :- call(X), call(X).
foo(X) :- call(X) -> call(X).


%%%%%%%%%%%%
%
%  used in section 8.10
%


a(1,2).
a(3,4).

