%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                              %
%   sec91.pro  test deending on the value of   %
%   max integer.                               %
%  Copyright  J.P.E Hodgson                    %
%             Saint Joseph's University        %
%             Philadelphia.   PA 19131         %
%                                              %
%    Tests of section 9.1 arithmetic           %
%    functions.                                %
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
%   Version for Calypso  1 oct 1998            %
%                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




test_92 :- 
        log('Testing max integer, section 9.2'), log_nl,
        (
        do_catch(get_current_prolog_flag(max_integer,MI),         
                 B,
                 (log('Unexected error '), log(B), log_nl, fail)
        )
       ->
       (
     	error_test(X52 is +(MI,1),evaluation_error),
     	error_test(X53 is -(+(MI,1),1),evaluation_error),
     	error_test(X54 is -(-1,MI),evaluation_error),
     	error_test(X55 is *(MI,2),evaluation_error),
     	R is float(MI) * 2,
     	error_test(X56 is floor(R),evaluation_error)
      )
       ;
     true
     ),
     log('Section 9.2 done.'), log_nl.
