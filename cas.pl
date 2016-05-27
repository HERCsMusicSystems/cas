/* -------------------------------------------------------------------
   CAS for SWI-Prolog version 7.2.3.
   
   Based originally on ideas in
     - SIMPSV.PRO (version 1.0), (c) 1987 S.Vaghi.
     - Elementary.mac (c) 2016 Chris Sangwin [https://github.com/maths/moodle-qtype_stack/blob/master/stack/maxima/elementary.mac]

   V1.0 2016-5-18.  Chris Sangwin
   -------------------------------------------------------------------
   	Example of how to use the program:
	    to tidy the expression (2*1)*(x^(2-1))
        (1) tidy( (2*1)*(x^(2-1)), Z).
        (2) Y = (2*1)*(x^(2-1)), tidy( Y, Z).
    In both cases a two pass tidying (simplification) is performed.
    ------------------------------------------------------------------ */

/*  Definition of operators. */
:- module(cas, [tidy/2, gather/2, distribute/2, factor/2]).

?-op( 9,  fx, 'unaryminus').        /*  Minus sign.         */
?-op(11, yfx, '^').                 /*  Exponentiation.     */
?-op(11,  fx, 'ln').                /*  Natural logarithm.  */

/*  Two pass very basic tidying (i.e. "simplification") clause.  */

tidy(X,Y) :- s(X, Z), s(Z, Y).

/*  List processing of the expression to be simplified.  */

s(X, X) :- atomic(X), ! .
s(X, Y) :- X =..[Op, Z], s(Z, Z1), u(Op, Z1, Y), ! .
s(X, Y) :- X =..[Op, Z, W], s(Z, Z1),
                 s(W, W1),
                 r(Op, Z1, W1, Y), ! .

/*  Simplification clauses for addition.  */

r('+', unaryminus(X), unaryminus(X), Z) :- b('*', 2, X, W), u('unaryminus', W, Z) , ! .
r('+', X, X, Z) :- b('*', 2, X, Z), ! .

r('+', X, unaryminus(Y), Z) :- b('-', X, Y, Z), ! .
r('+', unaryminus(X), Y, Z) :- b('-', Y, X, Z), ! .
r('+', unaryminus(X), unaryminus(Y), Z) :- b('+', X, Y, W), u('unaryminus', W, Z), ! .

r('+',   X, Y/Z, W) :- integer(X), integer(Y), integer(Z),
                       T is Z*X+Y,
                       b('/', T, Z, W), ! .
r('+', X/Z,   Y, W) :- integer(X), integer(Y), integer(Z),
                       T is X+Y*Z,
                       b('/', T, Z, W), ! .

r('+',   X, Y+Z, W) :- b('+', Y, Z, T), b('+', X, T, W), ! .
r('+', X+Y,   Z, W) :- b('+', X, Y, T), b('+', T, Z, W), ! .

r('+', X*Y ,Z*Y, W) :- b('+', X, Z, T), b('*', Y, T, W), ! .
r('+', X*Y ,Y*Z, W) :- b('+', X, Z, T), b('*', Y, T, W), ! .
r('+', Y*X ,Z*Y, W) :- b('+', X, Z, T), b('*', Y, T, W), ! .
r('+', Y*X ,Y*Z, W) :- b('+', X, Z, T), b('*', Y, T, W), ! .

r('+', X, Y, Z) :- integer(Y), b('+', Y, X, Z), ! .

/*  Simplification clauses for subtraction.  */

r('-', X, unaryminus(X), Z) :- b('*', 2, X, Z), ! .
r('-', unaryminus(X), X, Z) :- b('*', 2, X, W), u('unaryminus', W, Z), ! .

r('-', X, unaryminus(Y), Z) :- b('+', X, Y, Z), ! .
r('-', unaryminus(X), Y, Z) :- b('+', X, Y, W), u('unaryminus', W, Z), ! .
r('-', unaryminus(X), unaryminus(Y), Z) :- b('-', Y, X, Z), ! .

r('-',   X, Y/Z, W) :- integer(X), integer(Y), integer(Z),
                       T is X*Z-Y,
                       b('/', T, Z, W), ! .
r('-', X/Z,   Y, W) :- integer(X), integer(Y), integer(Z),
                       T is X-Y*Z,
                       b('/', T, Z, W), ! .

r('-',   X, Y-Z, W) :- b('-', Y, Z, T), b('-', X, T, W), ! .
r('-', X-Y,   Z, W) :- b('-', X, Y, T), b('-', T, Z, W), ! .

r('-', X*Y, Z*Y, W) :- b('-', X, Z, T), b('*', Y, T, W), ! .
r('-', X*Y, Y*Z, W) :- b('-', X, Z, T), b('*', Y, T, W), ! .
r('-', Y*X, Z*Y, W) :- b('-', X, Z, T), b('*', Y, T, W), ! .
r('-', Y*X, Y*Z, W) :- b('-', X, Z, T), b('*', Y, T, W), ! .

/*  Simplification clauses for multiplication.  */

r('*', X, X, Z) :- b('^', X, 2, Z), ! .
r('*', unaryminus(X),unaryminus(X), Z) :- b('^', X, 2, Z), ! .
r('*', unaryminus(X), X, Z) :- b('^', X, 2, W), u('unaryminus', W, Z), ! .
r('*',X ,unaryminus(X), Z) :- b('^', X, 2, W), u('unaryminus', W, Z), ! .

r('*',      X, X^unaryminus(1), Z) :- b('/', X, X, Z), ! .
r('*', X^unaryminus(1),      X, Z) :- b('/', X, X, Z), ! .

r('*',   X, 1/X, Z) :- b('/', X, X, Z), ! .
r('*', 1/X,   X, Z) :- b('/', X, X, Z), ! .
r('*',   X, 1/Y, Z) :- b('/', X, Y, Z), ! .
r('*', 1/X,   Y, Z) :- b('/', Y, X, Z), ! .
r('*',   M, N/X, Z) :- atomic(M), atomic(N),
                       b('*', M, N, S), b('/', S, X, Z), ! .
r('*', M/X,   N, Z) :- atomic(M), atomic(N),
                       b('*', M, N, S), b('/', S, X, Z), ! .


r('*',  X, N/Y, Z) :- atomic(N), b('/', X, Y, S), b('*', N, S, Z), ! .
r('*', N/Y,   X, Z) :- atomic(N), b('/', X, Y, S), b('*', N, S, Z), ! .

r('*',     X, Y^unaryminus(1), Z) :- b('/', X, Y, Z), ! .
r('*',     X,  X^unaryminus(Y),Z) :- b('-', Y, 1, S), b('^', X, S, T), b('/', 1, T, Z), ! . 
r('*', X^unaryminus(1),     Y, Z) :- b('/', Y, X, Z), ! .
r('*', X^unaryminus(Y),     X, Z) :- b('-', Y, 1, S), b('^', X, S, T), b('/', 1, T, Z), ! .

r('*',  X, X^Y, Z) :- b('+', 1, Y, S), b('^', X, S, Z), ! .
r('*', X^Y,  X, Z) :- b('+', Y, 1, S), b('^', X, S, Z), ! .

r('*', unaryminus(X), unaryminus(Y), Z) :- b('*', X, Y, Z), ! .
r('*', X,unaryminus(Y), Z) :- b('*', X, Y, W), u('unaryminus',W,Z), ! .
r('*', unaryminus(X), Y, Z) :- b('*', X, Y, W), u('unaryminus',W,Z), ! .

r('*',Z^unaryminus(X),Z^unaryminus(Y),W) :- b('+',X,Y,S), b('^',Z,S,T), b('/',1,T,W), ! .
r('*',Z^unaryminus(X), Z^Y,W) :- b('-',Y,X,S), b('^',Z,S,W), ! .
r('*', Z^X,Z^unaryminus(Y),W) :- b('-',X,Y,S), b('^',Z,S,W), ! .
r('*', Z^X, Z^Y,W) :- b('+',X,Y,T), b('^',Z,T,W), ! .
r('*',X^unaryminus(Z),Y^unaryminus(Z),W) :- b('*',X,Y,S), b('^',S,Z,T), b('/',1,T,W), ! .
r('*', X^Z,Y^unaryminus(Z),W) :- b('/',X,Y,S), b('^',S,Z,W), ! .
r('*',X^unaryminus(Z), Y^Z,W) :- b('/',Y,X,S), b('^',S,Z,W), ! .
r('*', X^Z, Y^Z,W) :- b('*',X,Y,T), b('^',T,Z,W), ! .

r('*', X*Y,   Y, Z) :- b('^',Y,2,S), b('*',X,S,Z), ! .
r('*', Y*X,   Y, Z) :- b('^',Y,2,S), b('*',X,S,Z), ! .
r('*',   Y, X*Y, Z) :- b('^',Y,2,S), b('*',X,S,Z), ! .
r('*',   Y, Y*X, Z) :- b('^',Y,2,S), b('*',X,S,Z), ! .

r('*', X*Y, X*Z, W) :- b('*',Y,Z,S), b('^',X,2,T), b('*',T,S,W), ! .
r('*', Y*X, X*Z, W) :- b('*',Y,Z,S), b('^',X,2,T), b('*',T,S,W), ! .
r('*', X*Y, Z*X, W) :- b('*',Y,Z,S), b('^',X,2,T), b('*',T,S,W), ! .
r('*', Y*X, Z*X, W) :- b('*',Y,Z,S), b('^',X,2,T), b('*',T,S,W), ! .

r('*',  M, N*X, W) :- atomic(M), atomic(N),
                      b('*',M,N,P), b('*',P,X,W), ! .
r('*',M*X,   N, W) :- atomic(M), atomic(N),
                      b('*',M,N,P), b('*',P,X,W), ! .

r('*',   X, Y*Z, W) :- b('*',Y,Z,T), b('*',X,T,W), ! .
r('*', X*Y,   Z, W) :- b('*',X,Y,T), b('*',T,Z,W), ! .

r('*',X,Y,Z) :- integer(Y), b('*',Y,X,Z), ! .

/*  Simplification clauses for division
           (division is never actually performed).  */

r('/', 1, X/Y, Z) :- b('/',Y,X,Z), ! .
r('/', unaryminus(1), X/Y, Z) :- b('/',Y,X,W), u('unaryminus',W,Z), ! .

r('/', unaryminus(X), unaryminus(Y), Z) :- b('/',X,Y,Z), ! .
r('/', X, unaryminus(Y), Z) :- b('/',X,Y,W), u('unaryminus',W,Z), ! .
r('/', unaryminus(X), Y, Z) :- b('/',X,Y,W), u('unaryminus',W,Z), ! .

r('/',      X, Y^unaryminus(1), Z) :- b('*',X,Y,Z), ! .
r('/', X^unaryminus(1),      Y, Z) :- b('*',X,Y,W), b('/',1,W,Z), ! .

r('/',   X, Y/Z, W) :- b('*',X,Z,T), b('/',T,Y,W), ! .
r('/', X/Y,   Z, W) :- b('*',Y,Z,T), b('/',X,T,W), ! .

r('/',     X,Y^unaryminus(Z), W) :- b('^',Y,Z,T), b('*',X,T,W), ! .
r('/', X^unaryminus(Z),     Y,W) :- b('^',X,Z,S), b('*',S,Y,T), b('/',1,T,W), ! .

r('/', X, X^unaryminus(Y),Z) :- b('+',1,Y,S), b('^',X,S,Z), ! .
r('/', X,   X^Y, Z) :- b('-',Y,1,S), b('^',X,S,T), b('/',1,T,Z), ! .
r('/', X^unaryminus(Y), X, Z) :- b('+',Y,1,S), b('^',X,S,T), b('/',1,T,Z), ! .

r('/',   X^Y,     X,Z) :- b('-',Y,1,S), b('^',X,S,Z), ! .
r('/',   X^N,X^unaryminus(M),Z) :- b('+',N,M,S), b('^',X,S,Z), ! .
r('/', X^unaryminus(N),   X^M,Z) :- b('+',N,M,S), b('^',X,S,T), b('/',1,T,Z), ! .
r('/', X^unaryminus(N), X^unaryminus(M),Z) :- b('-',M,N,S), b('^',X,S,Z), ! .
r('/',   X^N,   X^M,Z) :- b('-',N,M,W), b('^',X,W,Z), ! .

r('/',X^unaryminus(Z),   Y^Z,W) :- b('*',X,Y,S), b('^',S,Z,T), b('/',1,T,W), ! .
r('/',   X^Z,Y^unaryminus(Z),W) :- b('*',X,Y,S), b('^',S,Z,W), ! .
r('/',X^unaryminus(Z),Y^unaryminus(Z),W) :- b('/',Y,X,S), b('^',S,Z,W), ! .
r('/',   X^Z,   Y^Z,W) :- b('/',X,Y,T), b('^',T,Z,W), ! .


/*  Simplification clauses for exponentiation.  */

r('^', X, unaryminus(1), Y) :- b('/', 1, X, Y), ! .
r('^', X, unaryminus(Y), Z) :- b('^', X, Y, W), b('/', 1, W, Z), ! .
r('^', X^Y, Z, W) :- b('*',Y,Z,T), b('^', X, T, W), ! .

/*  Catch all clause to cover cases not covered by the previous clauses. */

r(X, Y, Z, W) :- b(X, Y, Z, W).

/*  Basic rules for the unary operator 'unaryminus'.  */
u('unaryminus', 0, 0) :- ! .
u('unaryminus', unaryminus(X), X) :- ! .
u('unaryminus', X, unaryminus(X)) :- ! .
u('unaryminus', X^Y, unaryminus(X^Y) ) :- ! .

/*  Basic rules of addition.  */

b('+', X, 0, X) :- ! .
b('+', unaryminus(X), 0, unaryminus(X)) :- ! .
b('+', 0, X, X) :- ! .
b('+', 0,unaryminus(X),unaryminus(X)) :- ! .
b('+', X,unaryminus(X), 0) :- ! .
b('+',unaryminus(X), X, 0) :- ! .
b('+', X, Y, Z) :- integer(X), integer(Y),
                Z is X+Y, ! .
b('+', X, Y, X+Y).

/*  Basic rules of subtraction.  */

b('-', X, 0, X) :- ! .
b('-', unaryminus(X), 0, unaryminus(X)) :- ! .
b('-', 0, X,unaryminus(X)) :- ! .
b('-', 0, unaryminus(X), X) :- ! .
b('-', unaryminus(X), unaryminus(X), 0) :- ! .
b('-', X, X, 0) :- ! .

b('-', X, Y, Z) :- integer(X), integer(Y),
                Z is X-Y, ! .

b('-', X, Y, X-Y).

/*  Basic rules of multiplication.  */

b('*', 0, _, 0) :- ! .
b('*', 0, unaryminus(_), 0) :- ! .
b('*', _, 0, 0) :- ! .
b('*', unaryminus(_), 0, 0) :- ! .

b('*', 1, X, X) :- ! .
b('*', 1, unaryminus(X), unaryminus(X)) :- ! .
b('*', unaryminus(1), X, unaryminus(X)) :- ! .
b('*', unaryminus(1), unaryminus(X), X) :- ! .
b('*', X, 1, X) :- ! .
b('*', unaryminus(X), 1, unaryminus(X)) :- ! .
b('*', X, unaryminus(1), unaryminus(X)) :- ! .
b('*', unaryminus(X), unaryminus(1), X) :- ! .

b('*', X/Y,   Y, X) :- ! .
b('*',   Y, X/Y, X) :- ! .

b('*',X,Y,Z) :- integer(X), integer(Y),
                Z is X*Y, ! .

b('*',X,Y,X*Y).

/*  Basic rules of division.  */

b('/', 0, 0, 'ERROR - indefinite form 0/0') :- ! .   /* Indefinite form. */
b('/', _, 0, 'ERROR - division by 0      ') :- ! .   /* Division by 0.   */

b('/', 0, _, 0) :- ! .
b('/', 0, unaryminus(_), 0) :- ! .

b('/', X, 1, X) :- ! .
b('/', unaryminus(X), 1, unaryminus(X)) :- ! .

b('/', 1, X,    1/X) :- ! .
b('/', unaryminus(1), X, unaryminus(1/X)) :- ! .

b('/', X, unaryminus(1), unaryminus(X)) :- ! .
b('/', unaryminus(X), unaryminus(1), X) :- ! .

b('/', 1,  unaryminus(X), unaryminus(1/X)) :- ! .
b('/', unaryminus(1),  unaryminus(X), 1/X) :- ! .
b('/', 1, 1/X, X) :- ! .
b('/', unaryminus(1), 1/X, unaryminus(X)) :- ! .

b('/', X, unaryminus(X), unaryminus(1)) :- ! .
b('/', unaryminus(X), X, unaryminus(1)) :- ! .
b('/', unaryminus(X), unaryminus(X), 1) :- ! .
b('/', X, X, 1) :- ! .

b('/', X, Y, X/Y).

/*  Basic rules of exponentiation.  */

b('^', 1, _, 1) :- ! .

/* Indefinite forms. */

b('^', 0, 0,'ERROR - indefinite form 0^0       ') :- ! .
b('^', 0, unaryminus(1), 'ERROR - indefinite form 0^unaryminus(1) = 1/0') :- ! .
b('^', 0, unaryminus(K), 'ERROR - indefinite form 0^unaryminus(K) = 1/0') :- atomic(K), ! .


b('^', unaryminus(X), 1, unaryminus(X)) :- ! .
b('^', X, 1, X) :- ! .
b('^', _, 0, 1) :- ! .

/* Recursive clauses to calculate the n-th power of positive and negative integers. */

b('^', X, N, Y)             :- integer(X), integer(N),
                             M is N-1, b('^', X, M, Z),
                             Y is Z*X, ! .
b('^', unaryminus(X), N, Y) :- integer(X), integer(N),
                             R is (N mod 2), R \= 0,
                             b('^', X, N, Z), Y = unaryminus(Z), ! .
b('^', unaryminus(X), N, Y) :- integer(X), integer(Y),
                           R is (N mod 2), R = 0,
                           b('^', X, N, Z), Y =  Z, ! .

b('^', unaryminus(X), Y, unaryminus(X^Y)) :- ! .

b('^', X, Y, X^Y).

/* Gather like terms.  */

gather(A*X+B*X, (A+B)*X) :- ! . 
gather(X+X,2*X) :- ! .
gather(A*X+X,(A+1)*X) :- ! .

/* Distribution of multiplication over addition. */
distribute(A*(X+Y), A*X+A*Y) :- ! .
distribute(A*(X+Y)+Z, A*X+A*Y+Z) :- ! .
distribute(A, A) :- ! .

factor(X, Y) :- distribute(Y, X).

/* Generate a random selection from a list.  */
rand(List, X) :- random_permutation(List, L), L= [X | _].
