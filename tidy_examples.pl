%% Examples of tidying up expressions for assessment purposes.
%% Imagine we are checking if the student has correctly expanded out the brackets on (x+N1)*(x+N2).

:-ensure_loaded(cas).

% Student's answer really is the same as the tidied up teacher's answer.
ass1(SA, N1, N2) :- TA = x^2+(N1+N2)*x+(N1*N2), tidy(TA, SA).              

% If we tidy up the student's answer it is the same as the teacher's.
ass2(SA, N1, N2) :- TA = x^2+(N1+N2)*x+(N1*N2), tidy(SA, Y), tidy(TA, Y).  

% Student's answer is algebraically equivalent to teacher's answer.
% We have used "tidy" in ass3, but we really want a stronger thing here.
ass3(SA, N1, N2) :- TA = x^2+(N1+N2)*x+(N1*N2), expand(SA-TA, Y), tidy(Y, Z), Z = 0.       

:- begin_tests(cas).

% ass1
test(cas) :- ass1(x^2+5*x+6, 3, 2).
test(cas, fail) :- ass1(x^2+6+5*x, 3, 2).
test(cas, fail) :- ass1(x^2+(2+3)*x+2*3, 3, 2).
test(cas, fail) :- ass1(x^2+2*x+3*x+6, 3, 2).
test(cas, fail) :- ass1((x+2)*(x+3), 3, 2).
test(cas, fail) :- ass1(x^2+2*x+3*x+7, 3, 2).

% ass2
test(cas) :- ass2(x^2+5*x+6, 3, 2).
test(cas) :- ass2(x^2+6+5*x, 3, 2).
test(cas) :- ass2(x^2+(2+3)*x+2*3, 3, 2).
test(cas) :- ass2(x^2+2*x+3*x+6, 3, 2).
test(cas, fail) :- ass2((x+2)*(x+3), 3, 2).
test(cas, fail) :- ass2(x^2+2*x+3*x+7, 3, 2).

% ass3
test(cas) :- ass3(x^2+5*x+6, 3, 2).
test(cas) :- ass3(x^2+6+5*x, 3, 2).
test(cas) :- ass3(x^2+(2+3)*x+2*3, 3, 2).
test(cas) :- ass3(x^2+2*x+3*x+6, 3, 2).
test(cas) :- ass3((x+2)*(x+3), 3, 2).
test(cas, fail) :- ass3(x^2+2*x+3*x+7, 3, 2).

:- end_tests(cas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Different types of "tidying".
%% Rules in order of increasing complexity.

% (0) Rules you almost always want to apply.  = compating.
% See, for example, https://github.com/maths/moodle-qtype_stack/blob/master/stack/maxima/elementary.mac
% 1*x -> x.
% x+0 -> x.
% etc.

% (1) Tidying.
% Order terms in sums and products.     
% e.g. x+z+y -> x+y+z
%      x+x^2+1 -> x^2+x+1.

% (2) Minial changing of expressions.
% Integer arithmatic, e.g 2+3->5.
% Gathering like terms   2*x^3+3*x^3 -> 5*x^3.

% (3) Algebraic Equivalence.

    
    