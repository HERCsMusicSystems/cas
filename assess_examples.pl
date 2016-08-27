%% Examples of manipulating expressions for assessment purposes.
%% Imagine we are checking if the student has correctly expanded out the brackets on (x+N1)*(x+N2).

:-ensure_loaded(cas).


% Student's answer (S) really is the same as the tidied up teacher's answer (T).
% "Tidy" here means both idenity transformations such as 0*x->0 and numerical arithmatic.
% There is no associativity or commutativity so if the student has the wrong order in terms, their answer fails this test.
ass1(S, T) :- 
    idRules(Idr),
    apply_rule_set_repeat([numArith | Idr], T, T1),
    equal_exact(S, T1).


% We do arithmatic and trivial operations in both student's answer and teacher's answer
% There is still no associativity or commutativity so if the student has the wrong order in terms, their answer fails this test.
ass2(S, T) :- 
    idRules(Idr),
    apply_rule_set_repeat([numArith | Idr], T, T1),
    apply_rule_set_repeat([numArith | Idr], S, S1),
    equal_exact(S1, T1).

% Here we allow associativity or commutativity, but don't "tidy" the student's arithmatic at all.
ass3(S, T) :- 
    idRules(Idr),
    apply_rule_set_repeat([numArith | Idr], T, T1),
    equal_com_ass(S, T1).

% Here we allow associativity or commutativity, and we do "tidy" the student's arithmatic.
% This is close to algebraic equivalence, but we haven't gathered like terms yet.
ass4(S, T) :- 
    idRules(Idr),
    apply_rule_set_repeat([numArith | Idr], T, T1),
    apply_rule_set_repeat([numArith | Idr], S, S1),
    equal_com_ass(S1, T1).

:- begin_tests(assess_examples).

% ass1
test(assess_examples) :- ass1(x^2+5*x+6, x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass1(x^2+(2+3)*x+2*3, x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass1(x^2+6+5*x, x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass1(x^2+2*x+3*x+6, x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass1((x+2)*(x+3), x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass1(x^2+2*x+3*x+7, x^2+(2+3)*x+2*3).

% We can easily include random parameters here.
test(assess_examples) :- N1 is 3, N2 is 2, ass1(x^2+5*x+6, x^2+(N1+N2)*x+N1*N2).
test(assess_examples) :- N1 is 3, N2 is 3, ass1(x^2+6*x+9, x^2+(N1+N2)*x+N1*N2).

test(assess_examples) :- N1 is 1, N2 is 1, ass1(x^2+2*x+1, x^2+(N1+N2)*x+N1*N2).
test(assess_examples, fail) :- N1 is 1, N2 is 1, ass1(x^2+x+x+1, x^2+(N1+N2)*x+N1*N2).
test(assess_examples, fail) :- N1 is 1, N2 is 1, ass1(x^2+2*x+1*1, x^2+(N1+N2)*x+N1*N2).

% This example illustrates the identity rules being applied to the teachers answer.
test(assess_examples) :- N1 is 1, ass1(x^2+x, x^2+N1*x).
test(assess_examples, fail) :- N1 is 1, ass1(x^2+1*x, x^2+N1*x).

% Note, we haven't included power laws yet, so x*x is not yet x^2.
test(assess_examples, fail) :- ass1(x*x, x^2).

% ass2
test(assess_examples) :- ass2(x^2+5*x+6, x^2+(2+3)*x+2*3).
% The following test is the difference between ass1 and ass2.
test(assess_examples) :- ass2(x^2+(2+3)*x+2*3, x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass2(x^2+6+5*x, x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass2(x^2+2*x+3*x+6, x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass2((x+2)*(x+3), x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass2(x^2+2*x+3*x+7, x^2+(2+3)*x+2*3).

% ass3
test(assess_examples) :- ass3(x^2+5*x+6, x^2+(2+3)*x+2*3).
% Unlike in ass2, we don't do the arithmetic here, so this fails again.
test(assess_examples, fail) :- ass3(x^2+(2+3)*x+2*3, x^2+(2+3)*x+2*3).
% The following test is the difference between ass1 and ass2.
test(assess_examples) :- ass3(x^2+6+5*x, x^2+(2+3)*x+2*3).
% We are still not adding terms.
test(assess_examples, fail) :- ass3(x^2+2*x+3*x+6, x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass3((x+2)*(x+3), x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass3(x^2+2*x+3*x+7, x^2+(2+3)*x+2*3).

% ass4
test(assess_examples) :- ass4(x^2+5*x+6, x^2+(2+3)*x+2*3).
test(assess_examples) :- ass4(x^2+(2+3)*x+2*3, x^2+(2+3)*x+2*3).
test(assess_examples) :- ass4(x^2+6+5*x, x^2+(2+3)*x+2*3).
test(assess_examples) :- ass4(2*3+x^2+(2+3)*x, x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass4(x^2+2*x+3*x+6, x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass4((x+2)*(x+3), x^2+(2+3)*x+2*3).
test(assess_examples, fail) :- ass4(x^2+2*x+3*x+7, x^2+(2+3)*x+2*3).

:- end_tests(assess_examples).

