/* 

An answer test is used to compare two expressions to establish whether they are equal.

Chris Sangwin, August 2016.

*/

% Strictly speaking we probably don't need this function, but it makes sense when authoring a 
% question to specify the property we want is "exactly equal".  This is much less cryptic.
equal_exact(S, S) :- !.
equal_exact(_S, _T) :- !, fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(equal_exact).

test(equal_exact) :- equal_exact(x, x).
test(equal_exact, fail) :- equal_exact(x, y).
test(equal_exact, fail) :- equal_exact(x-x, 0).

:- end_tests(equal_exact).
