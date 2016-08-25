%% Examples of tidying up expressions for assessment purposes.
%% Imagine we are checking if the student has correctly expanded out the brackets on (x+N1)*(x+N2).

:-ensure_loaded(cas).

% Student's answer really is the same as the tidied up teacher's answer.
ass1(SA, N1, N2) :- TA = x^2+(N1+N2)*x+(N1*N2), tidy(TA, SA).              

% If we tidy up the student's answer it is the same as the teacher's.
ass2(SA, N1, N2) :- TA = x^2+(N1+N2)*x+(N1*N2), tidy(SA, Y), tidy(TA, Y).  

% Student's answer is algebraically equivalent to teacher's answer.
ass3(SA, N1, N2) :- TA = x^2+(N1+N2)*x+(N1*N2), tidy(SA-TA, Z), Z = 0.       

:- begin_tests(cas).
:- use_module(cas).

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
