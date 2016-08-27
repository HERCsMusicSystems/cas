/* Unit tests of the CAS.pl library.  */
/* http://www.swi-prolog.org/pldoc/man?section=unitbox  */
/* run_tests.                                           */

:- begin_tests(cas).
:- use_module(cas).

/* 
Task 1;
Use the tests from the 1994 paper "Review of CAS mathematical capabilities", by
Michael Wester, available for example here: http://www.math.unm.edu/~wester/cas/book/Wester.pdf And write tests for each of the example in there if sympy can do it, or a
new issue if it cannot. Together with fixing the issue 3787 , this should
produce a nice documentation of how to do the ususal things in sympy.
*/

/* Identity operators. */
test(cas) :- tidy(0+x, x). 
test(cas) :- tidy(1*x, x). 
test(cas) :- tidy(0*x, 0). 
test(cas) :- tidy(x*0, 0). 
test(cas) :- tidy(x^1, x). 
test(cas) :- tidy(x^0, 1). 

test(cas) :- tidy(x/x, 1). 
test(cas) :- tidy((x+a)/(x+a), 1). 
test(cas) :- tidy((x^2+a*x+2)/(x^2+a*x+2), 1). 

test(cas) :- tidy((x-1)^2/(x^2-2*x+1), 1). 

/* Numbers.  */
test(cas) :- tidy(1 + 2, 3). 

/* Grouping terms. */
test(cas) :- tidy(x+x, 2*x). 
test(cas) :- tidy(x+x+x, 3*x). 
test(cas) :- tidy((x-1)^2+3*(x-1)^2, 4*(x-1)^2). 

/* Order: numbers in products should go first. */
test(cas) :- tidy(2*x, 2*x). 
test(cas) :- tidy(x*2, 2*x). 
test(cas) :- tidy(x*3*y, 3*x*y). 
test(cas) :- tidy(y*3*x, 3*x*y). 


/* Logarithms: not yet implemented.
test(cas) :- tidy((1-sqrt(2))^ln(1-sqrt(2)), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^ln(2), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^(ln(ln(1-sqrt(2)))), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^ln(ln(2)), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^(1-sqrt(2))^(x+1)*ln(2)*ln(1-sqrt(2)), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^ln(1-sqrt(2)), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^ln(2), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^ln(ln(1-sqrt(2))), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^ln(ln(2)), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^((1-sqrt(2))^(x+1)*ln(2)*ln(1-sqrt(2))), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^((1-sqrt(2))^(x+1)*ln(2)*ln(1-sqrt(2))), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^(ln(1-sqrt(2))), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^ln(2), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^ln(ln(1-sqrt(2))), sqrt(2)).
test(cas) :- tidy((1-sqrt(2))^ln(ln(2)), sqrt(2)).
*/

/* Gather like terms. */
test(cas) :- gather(x+x, 2*x). 
test(cas) :- gather(x+x+x, 3*x). 
test(cas) :- gather(x+x+y, 2*x+y). 
test(cas) :- gather(x+y+x, 2*x+y). 


/* Expand. */
test(cas) :- expand(x, x). 
test(cas) :- expand(x*(a+b), x*a+x*b). 
test(cas) :- expand(x*(a+b+c), x*a+x*b+x*c). 
test(cas) :- expand(2*x*(a+b), 2*x*a+2*x*b). 
test(cas) :- expand(x*(a+b)+2, x*a+x*b+2). 
test(cas) :- expand((x+y)*(a+b+c), x*a+x*b+x*c+y*a+y*b+y*c). 

:- end_tests(cas).


