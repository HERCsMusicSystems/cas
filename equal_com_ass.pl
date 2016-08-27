/* 

An answer test is used to compare two expressions to establish whether they satisfy some mathematical criteria. 
This file is a prototype answer test which establishes equality up to associativity and commutativity.

For example a+b=b+a but x+x\neq 2x.

Chris Sangwin, August 2016.

*/

equal_com_ass(Sa, Ta) :-
    apply_rule_set_repeat([to_nary, narySort], Sa, S),
    apply_rule_set_repeat([to_nary, narySort], Ta, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(equal_com_ass).

test(equal_com_ass) :- equal_com_ass(1,1).
test(equal_com_ass, fail) :- equal_com_ass(1,0).

% Numerical examples.
test(equal_com_ass, fail) :- equal_com_ass(1/0, 0).
test(equal_com_ass, fail) :- equal_com_ass(0, 1/0).
test(equal_com_ass, fail) :- equal_com_ass(2/4, 1/2).
test(equal_com_ass, fail) :- equal_com_ass(3^2, 8).
test(equal_com_ass, fail) :- equal_com_ass(3^2, 9).
test(equal_com_ass, fail) :- equal_com_ass(4^(1/2), 2).
test(equal_com_ass, fail) :- equal_com_ass(1/3^(1/2), (1/3)^(1/2)).
test(equal_com_ass, fail) :- equal_com_ass(sqrt(3)/3, (1/3)^(1/2)).
test(equal_com_ass, fail) :- equal_com_ass(sqrt(3), 3^(1/2)).
test(equal_com_ass, fail) :- equal_com_ass(2*sqrt(2), sqrt(8)).
test(equal_com_ass, fail) :- equal_com_ass(2*2^(1/2), sqrt(8)).
test(equal_com_ass, fail) :- equal_com_ass(sqrt(2)/4, 1/sqrt(8)).
test(equal_com_ass, fail) :- equal_com_ass(1/sqrt(2), 2^(1/2)/2).

% Basic algebraic expressions. 
test(equal_com_ass, fail) :- equal_com_ass(1*x, x).
test(equal_com_ass, fail) :- equal_com_ass(x+0, x).
test(equal_com_ass, fail) :- equal_com_ass(x^1, x).
test(equal_com_ass, fail) :- equal_com_ass(a^2/b^3, a^2*b^(-3)).
test(equal_com_ass) :- equal_com_ass(1+2*x, x*2+1).
test(equal_com_ass, fail) :- equal_com_ass(1+x, 2*x+1).
test(equal_com_ass, fail) :- equal_com_ass(1+x+x, 2*x+1).
test(equal_com_ass) :- equal_com_ass((x+y)+z, z+x+y).
test(equal_com_ass, fail) :- equal_com_ass(x*x, x^2).
test(equal_com_ass) :- equal_com_ass((x+5)*x, x*(5+x)).
test(equal_com_ass, fail) :- equal_com_ass(x*(x+5), 5*x+x^2).
test(equal_com_ass) :- equal_com_ass(3*(1+x)^2, (x+1)^2*3).
% The following are not considered equal because they need to use even powers of negative numbers being equal.
test(equal_com_ass, fail) :- equal_com_ass((1-x)^2, (x-1)^2).
test(equal_com_ass, fail) :- equal_com_ass((a-x)^6000, (x-a)^6000).

% Unary minus.
test(equal_com_ass) :- equal_com_ass(-1+2, 2-1).
test(equal_com_ass) :- equal_com_ass(-1*2+3*4, 3*4-1*2).
test(equal_com_ass, fail) :- equal_com_ass((-1*2)+3*4, 10).
test(equal_com_ass) :- equal_com_ass(-1*2+3*4, 3*4-1*2).
test(equal_com_ass) :- equal_com_ass(-x+y, y-x).
test(equal_com_ass) :- equal_com_ass(x*(-y), -x*y).
test(equal_com_ass) :- equal_com_ass(x*(-y), -(x*y)).
% The following two tests should fail because these require other rules, e.g. "-*- -> +".
% So, they are not equal up to associativity and commutativity.
test(equal_com_ass, fail) :- equal_com_ass((-x)*(-x), x*x).
test(equal_com_ass, fail) :- equal_com_ass((-x)*(-x), x^2).

% Rational expressions.
test(equal_com_ass, fail) :- equal_com_ass(1/2, 3/6).
test(equal_com_ass) :- equal_com_ass(1/(1+2*x), 1/(2*x+1)).
test(equal_com_ass, fail) :- equal_com_ass(2/(4+2*x), 1/(x+2)).
test(equal_com_ass) :- equal_com_ass((a*b)/c, a*(b/c)).
test(equal_com_ass) :- equal_com_ass((-x)/y, -(x/y)).
test(equal_com_ass) :- equal_com_ass(x/(-y), -(x/y)).
test(equal_com_ass, fail) :- equal_com_ass(-1/(1-x), 1/(x-1)).
test(equal_com_ass) :- equal_com_ass(1/2*1/x, 1/(2*x)).
test(equal_com_ass) :- equal_com_ass((k+8)/(k^2+4*k-12), (k+8)/(k^2+4*k-12)).
test(equal_com_ass) :- equal_com_ass((k+8)/(k^2+4*k-12), (k+8)/((k-2)*(k+6))).
test(equal_com_ass, fail) :- equal_com_ass((k+7)/(k^2+4*k-12), (k+8)/(k^2+4*k-12)).
test(equal_com_ass) :- equal_com_ass(-(2*k+6)/(k^2+4*k-12), -(2*k+6)/(k^2+4*k-12)).
test(equal_com_ass) :- equal_com_ass((1/2)*(a+b), (a+b)/2).

:- end_tests(equal_com_ass).
