% An individual rule based approach to expression manipulation.
%
% Chris Sangwin, August 2016.
%
% To find the data structure underlying an expression in prolog.
% writef('%d', [<expr>]).

module(cas_rules, [zeroAdd/2, map/3]).

% Each rule has a predicate ending in "p" and the function itself which performs the rule.
% NOTE: all these rules go one way only!  Therefore no circular loops.
zeroAdd(0+A, A) :- !.
zeroAdd(A+0, A) :- !.
zeroAdd(0+A, 0+A) :- !, fail.
zeroAdd(A, A).

zeroMul(0*_A, 0) :- !.
zeroMul(A, A).

oneMul(1*A, A) :- !.
oneMul(1*A, 1*A) :- !, fail.
oneMul(A, A).

onePow(1^_A, 1) :- !.
onePow(1^A, 1^A) :- !, fail.
onePow(A, A).

idPow(A^1, A) :- !.
idPow(A^1, A^1) :- !, fail.
idPow(A, A).

zeroPow(0^_A, 0) :- !.
zeroPow(0^A, 0^A) :- !, fail.
zeroPow(A, A).

zPow(_^0, 1) :- !.
zPow(A^0, A^0) :- !, fail.
zPow(A, A).

idNeg(A-A, 0) :- !.
idNeg(A-A, A-A) :- !, fail.
idNeg(A, A).

% List of all basic identity rules.
idRules([zeroAdd, zeroMul, oneMul, onePow, idPow, zeroPow, zPow, idNeg]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Do numerical arithmetic, with all four operations. */

numArith(+(A, B), S) :-
    number(A),
    number(B),
    S is A+B,
    !.
numArith(-(A, B), S) :-
    number(A),
    number(B),
    S is A-B,
    !.
numArith(*(A, B), S) :-
    number(A),
    number(B),
    S is A*B,
    !.
numArith(/(A, B), S) :-
    number(A),
    number(B),
    S is A/B,
    !.
numArith(^(A, B), S) :-
    number(A),
    number(B),
    S is A^B,
    !.
numArith(A, A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* 
  Distribute multiplication over addition. 
  (a+b)*c -> a*c + b*c.
  a*(b+c) -> a*b + a*c.
*/

/* TODO: deal with unary minus here... */
distMullAdd(*(+(A, B), C), A*C + B*C) :- !.
distMullAdd(*(A, +(B, C)), A*B + A*C) :- !.
distMullAdd(A, A).

/* TODO: this is prototype behaviour, but it exhibits the potential. */
expand(E, S) :- 
    apply_rule_set_repeat([from_nary, distMullAdd, numArith, gatherPow, gatherMul, to_nary, narySort], E, S1),
    apply_rule_set_repeat([from_nary], S1, S).

/* TODO:
  The reason we want to deal with nary operators is exemplified in the following examples:
  expand((x+2)*(x+3), S).
  expand((x+2)*(x+3)*(x+4), S).

  Having a binary + & * means we can't match up T+2*x+3*x.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gather terms in a product.

/* TODO: this is probably better done by manipulating nary terms. */
gatherMul(+(A, A), 2*A) :- !.
gatherMul(+(N*A, A), (N+1)*A) :- !.
gatherMul(+(A*N, A), (N+1)*A) :- !.
gatherMul(+(A, N*A), (1+N)*A) :- !.
gatherMul(+(A, A*N), (1+N)*A) :- !.
gatherMul(+(N*A, M*A), (N+M)*A) :- !.
gatherMul(+(A*N, M*A), (N+M)*A) :- !.
gatherMul(+(N*A, A*M), (N+M)*A) :- !.
gatherMul(+(A*N, A*M), (N+M)*A) :- !.
gatherMul(A,A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gather powers.  a^n*a^m -> a^(n+m)

/* TODO: combine terms in nary products as well. */
gatherPow(*(A, A), A^2) :- !.
gatherPow(*(A^N, A), A^(N+1)) :- !.
gatherPow(*(A, A^N), A^(1+N)) :- !.
gatherPow(*(A^N, A^M), A^(N+M)) :- !.
gatherPow(A,A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* To represent associative addition and multiplication we have an "nary" operator. 
  nary(<op>, <L>)
  where <op> is either '+' or '*'.
        <L> is a list of arguments.
*/

to_nary(E,E) :- atomic(E), !.
to_nary(+(A, B), nary(+, N)) :- to_nary_helper(+, +(A, B), N), !.
to_nary(*(A, B), nary(*, N)) :- to_nary_helper(*, *(A, B), N), !.
to_nary(E,E).

to_nary_helper(Op, E, N) :-
        E =.. [Op, A, B],
    to_nary_helper(Op, A, N1),
    to_nary_helper(Op, B, N2),
    append(N1, N2, N), !.
to_nary_helper(_, E, [E]).

/* Turns an nary term back into sums and products. */
% This should hold: from_nary(A, B) :- to_nary(B, A).
from_nary(E, E) :- atomic(E), !.
from_nary(nary(Op, L), S) :-
    reverse(L, R),
    from_nary_helper(Op, R, S), !.
from_nary(E, E).

from_nary_helper(_Op, [N], N) :- !.
from_nary_helper(Op, [N | L], S) :-
    S =.. [Op, E1, N],
    from_nary_helper(Op, L, E1).

% Sort the arguments of nary operators into a definite order. 
% This rule implements commuataivity of + and *, allowing us to sort 
% operands into a list.  We can then compare the sorted lists.
narySort(nary(Op, N), nary(Op, Ns)) :- 
    msort(N, Ns), % Warning: sort removes duplicates.
    !.
narySort(A, A). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Map an operator onto a list of expressions.
map(_, [], []) :- !.
map(R, [X | L], [X1 | L1]) :- 
    X1 =.. [R, X],
    map(R, L, L1).

% Apply a single rule.
apply(Rule, Expression, Result) :- 
   E1 =.. [Rule, Expression, Result],
   E1.

% Map a rule onto a list of expressions.
map_rule_list(_, [], []) :- !.
map_rule_list(R, [X | L], [X1 | L1]) :- 
    apply_rule(R, X, X1),
    map_rule_list(R, L, L1).

% Recurse rule accross expression tree.
% apply_rule(Rule, Expression, SimplifiedResult)
apply_rule(_, E, E) :- atomic(E), !.
apply_rule(R, E, S) :-
    apply(R, E, Er),
    Er =.. [Op | Args],
    map_rule_list(R, Args, Argsr),
    S =.. [Op | Argsr].

% This is very similar to the map_rule, but we use apply_rule instead.
apply_rule_helper(_, [], []) :- !.
apply_rule_helper(R, [X | L], [X1 | L1]) :- 
    XA =.. [apply_rule, R, X, X1],
    XA,
    apply_rule_helper(R, L, L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Apply a set of rules to an expression.
% apply_rule_set(RuleSet, Expression, SimplifiedResult)
apply_rule_set(_, E, E) :- atomic(E), !.
apply_rule_set([], E, E) :- !.
apply_rule_set([R | Rs], E, S) :- 
    apply_rule(R, E, Er),
    apply_rule_set(Rs, Er, S).

% Apply a set of rules to an expression repeatedly until it stops changing.
apply_rule_set_repeat(R, E, S) :- apply_rule_set_repeat_helper(R, E, [], S).

apply_rule_set_repeat_helper(_R, E, E, S) :- !, E=S.
apply_rule_set_repeat_helper(R, E, _Ep, S) :-
    apply_rule_set(R, E, Er),
    !,
    apply_rule_set_repeat_helper(R, Er, E, S). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Substitute (Bratko, pg 157).  */

% substitute(Subterm, Term, Subterm1, Term1)
% if all occurances of Subterm in Term are subsituted with Subterm1 then we get Term1.

% Case 1: Substitute the whole term.
substitute(Term, Term, Term1, Term1) :- !.

% Case 2: Nothing to substitute if Term is atomic.
substitute(_, Term, _, Term) :- atomic(Term), !.

% Case 3: Do substitution on arguments.
substitute(Sub, Term, Sub1, Term1) :-
  Term =.. [F | Args],
  sublist(Sub, Args, Sub1, Args1),
  Term1 =.. [F | Args1].

sublist(_, [], _, []).
sublist(Sub, [Term | Terms], Sub1, [Term1 |Terms1]) :-
  substitute(Sub, Term, Sub1, Term1),
  sublist(Sub, Terms, Sub1, Terms1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(cas_rules).

test(cas_rules) :- zeroAdd(0+a, a).
test(cas_rules) :- zeroAdd(0+sin(x), sin(x)).
test(cas_rules) :- zeroAdd(0, 0).
test(cas_rules) :- zeroAdd(a+0, a+0).

test(cas_rules) :- map(f, [], []).
test(cas_rules) :- map(f, [a,b,c], [f(a), f(b), f(c)]).

test(cas_rules) :- map_rule_list(zeroAdd, [], []).
test(cas_rules) :- map_rule_list(zeroAdd, [0+a], [a]).
test(cas_rules) :- map_rule_list(zeroAdd, [a+0], [a]).
test(cas_rules, fail) :- map_rule_list(zeroAdd, [0+a], [0+a]).
test(cas_rules) :- map_rule_list(zeroAdd, [0+a,b,0+sin(c)], [a,b,sin(c)]).

test(cas_rules) :- apply(zeroAdd, a, a).
test(cas_rules) :- apply(zeroAdd, 0+a, a).
test(cas_rules) :- apply(zeroAdd, a+0, a+0).

test(cas_rules) :- apply_rule(zeroAdd, a, a).
test(cas_rules) :- apply_rule(zeroAdd, 1, 1).
test(cas_rules) :- apply_rule(zeroAdd, 1+1, 1+1).
test(cas_rules) :- apply_rule(zeroAdd, 0.5, 0.5).

test(cas_rules) :- apply_rule(zeroAdd, 3*a, 3*a).
test(cas_rules) :- apply_rule(zeroAdd, 3*(0+a), 3*a).
test(cas_rules) :- apply_rule(zeroAdd, (0+b)*(0+a), b*a).
test(cas_rules) :- apply_rule(zeroAdd, (0+b)*(0+a)+7, b*a+7).
test(cas_rules) :- apply_rule(zeroAdd, a+0+b, a+b).

test(cas_rules) :- apply_rule(zeroMul, 0*(0+a), 0).
test(cas_rules) :- apply_rule(zeroMul, 0*a+b, 0+b).

test(cas_rules) :- apply_rule(oneMul, 1*(1+a), 1+a).
test(cas_rules) :- apply_rule(oneMul, 1*a+b, a+b).

test(cas_rules) :- apply_rule(numArith, 1+1, 2).
test(cas_rules) :- apply_rule(numArith, 2*4, 8).
test(cas_rules) :- apply_rule(numArith, 9/3, 3).
test(cas_rules) :- apply_rule(numArith, 3-9, -6).
test(cas_rules) :- apply_rule(numArith, 2^4, 16).
test(cas_rules) :- apply_rule_set_repeat([numArith], 2^4, 16).
test(cas_rules) :- apply_rule_set_repeat([numArith], 2^4+1, 17).

test(cas_rules) :- apply_rule(gatherPow, x*x, x^2).
test(cas_rules) :- apply_rule(gatherPow, x^3*x, x^(3+1)).
test(cas_rules) :- apply_rule(gatherPow, x*x^3, x^(1+3)).
test(cas_rules) :- apply_rule(gatherPow, a^n*a^m, a^(n+m)).
test(cas_rules) :- apply_rule(gatherPow, (x+1)^n*(x+1)^m, (x+1)^(n+m)).

test(cas_rules) :- apply_rule_set_repeat([gatherPow,numArith], a*a*a, a^3).

test(cas_rules) :- apply_rule_set([zeroAdd,zeroMul], 0*a+b, 0+b).
test(cas_rules) :- apply_rule_set([zeroMul,zeroAdd], 0*a+b, b).
test(cas_rules) :- apply_rule_set([zeroAdd,zeroMul], 0*b+0+a, 0+a).
test(cas_rules) :- apply_rule_set([zeroMul,zeroAdd], 0*b+0+a, 0+a).
test(cas_rules) :- apply_rule_set([zeroAdd,zeroMul], sin(0*a+b), sin(0+b)).

test(cas_rules) :- apply_rule_set_repeat([zeroAdd,zeroMul], 0*b+0+a, a).
test(cas_rules) :- apply_rule_set_repeat([zeroMul,zeroAdd], 0*b+0+a, a).
test(cas_rules) :- apply_rule_set_repeat([zeroAdd,zeroMul], 1+1, 1+1).
test(cas_rules) :- apply_rule_set_repeat([oneMul], 1*b+a, b+a).
test(cas_rules) :- apply_rule_set_repeat([oneMul], 1*1*b+0+a, b+0+a).
test(cas_rules) :- apply_rule_set_repeat([zeroAdd,oneMul], 1*1*b+0+a, b+a).
test(cas_rules, fail) :- apply_rule_set_repeat([zeroAdd,zeroMul], 1+1, 2).
test(cas_rules) :- idRules(X), apply_rule_set_repeat(X, a^0+(0+b)+(1*c), 1+b+c).
test(cas_rules) :- idRules(X), apply_rule_set_repeat(X, a^0-b^0, 0).

test(cas_rules) :- apply_rule_set_repeat([distMullAdd], c*(a+b), c*a+c*b).
test(cas_rules) :- apply_rule_set_repeat([distMullAdd], (a+b)*c, a*c+b*c).
test(cas_rules) :- apply_rule_set_repeat([distMullAdd], (a+b+c)*d, a*d+b*d+c*d).
test(cas_rules) :- apply_rule_set_repeat([distMullAdd], d*(a+b+c), d*a+d*b+d*c).
test(cas_rules) :- apply_rule_set_repeat([distMullAdd], (a+b)*(c+d),  a*c+a*d+(b*c+b*d)).
test(cas_rules) :- apply_rule_set_repeat([distMullAdd], (x+1)*(x+1),  x*x+x*1+(1*x+1*1)).

test(cas_rules) :- to_nary(a,a).
test(cas_rules) :- to_nary(a+b, nary(+, [a,b])).
test(cas_rules) :- to_nary(a+a, nary(+, [a,a])).
test(cas_rules) :- to_nary(a*a, nary(*, [a,a])).
test(cas_rules) :- to_nary(a+b+c, nary(+, [a,b,c])).
test(cas_rules) :- to_nary((a+b)+c, nary(+, [a,b,c])).
test(cas_rules) :- to_nary(a+(b+c), nary(+, [a,b,c])).
test(cas_rules) :- to_nary(a+4*b^2+c, nary(+, [a,4*b^2,c])).
test(cas_rules) :- to_nary((a*b)*c, nary(*, [a,b,c])).

test(cas_rules) :- from_nary(nary(*, [a,b,c]), (a*b)*c).
test(cas_rules) :- from_nary(nary(+, [a,4*b^2,c]), a+4*b^2+c).

test(cas_rules) :- narySort(nary(*, [b,a]), nary(*, [a,b])).
test(cas_rules) :- narySort(nary(*, [x,3,x]), nary(*, [3,x,x])).
test(cas_rules) :- narySort(nary(*, [x,x]), nary(*, [x,x])).
test(cas_rules, fail) :- narySort(nary(*, [x,x]), nary(*, [x])).


test(cas_rules) :- apply_rule_set_repeat([to_nary], sin(a+b+c), sin(nary(+, [a, b, c]))).
test(cas_rules) :- apply_rule_set_repeat([to_nary], 2*(a+b+b), nary(*, [2, nary(+, [a, b, b])])).

test(cas_rules) :- apply_rule_set_repeat([to_nary, narySort], 2*(z+a+y+0), nary(*, [2, nary(+, [0, a, y, z])])).

:- end_tests(cas_rules).
