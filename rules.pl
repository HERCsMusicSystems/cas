% An individual rule based approach to expression manipulation.
%
% Chris Sangwin, August 2016.

module(cas_rules, [zeroAdd/2, map/3]).

to_nary(+(A, B), N) :-
	to_nary(A, N1),
	to_nary(B, N2),
	append(N1, N2, N), !.

to_nary(E, [E]).

% Each rule has a predicate ending in "p" and the function itself which performs the rule.
% NOTE: all these rules go one way only!  Therefore no circular loops.
zeroAddp(0+_, true) :- ! .
zeroAddp(_, false).
zeroAdd(0+A, A) :- !.
zeroAdd(0+A, 0+A) :- !, fail.
zeroAdd(A, A).

zeroMulp(0*_, true) :- ! .
zeroMulp(_, false).
zeroMul(0*_A, 0) :- !.
zeroMul(A, A).

oneMulp(1*_, true) :- ! .
oneMulp(_, false).
oneMul(1*A, A) :- !.
oneMul(1*A, 1*A) :- !, fail.
oneMul(A, A).

onePowp(1^_, true) :- ! .
onePowp(_, false).
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

% List of all basic identity rules.
idRules([zeroAdd, zeroMul, oneMul, onePow, idPow, zeroPow, zPow]).

/* Premaure
orderPlus(A+B, S) :-
   sort([A,B], Sa),
   S =.. ['+' | Sa], !.
orderPlus(A, A).

orderMul(A*B, S) :- 
   sort([A,B], Sa),
   S =.. ['*' | Sa], !.
orderMul(A, A).

orderRules([orderPlus, orderMul]).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* To represent associative addition we have an "nary" operator. 
  nary(<op>, <L>)
  where <op> is either '+' or '*'.
        <L> is a list of arguments.
*/

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

% This is a clone of map_rule, but we use apply_rule instead.
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

test(cas_rules) :- zeroAddp(0+a, true).
test(cas_rules) :- zeroAdd(0+a, a).

test(cas_rules) :- zeroAddp(0+sin(x), true).
test(cas_rules) :- zeroAdd(0+sin(x), sin(x)).

test(cas_rules) :- zeroAddp(0, false).
test(cas_rules) :- zeroAdd(0, 0).

test(cas_rules) :- zeroAddp(a+0, false).
test(cas_rules) :- zeroAdd(a+0, a+0).

test(cas_rules) :- map(f, [], []).
test(cas_rules) :- map(f, [a,b,c], [f(a), f(b), f(c)]).

test(cas_rules) :- map_rule_list(zeroAdd, [], []).
test(cas_rules) :- map_rule_list(zeroAdd, [0+a], [a]).
test(cas_rules) :- map_rule_list(zeroAdd, [a+0], [a+0]).
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

test(cas_rules) :- apply_rule_set([zeroAdd,zeroMul], 0*a+b, 0+b).
test(cas_rules) :- apply_rule_set([zeroMul,zeroAdd], 0*a+b, b).
test(cas_rules) :- apply_rule_set([zeroAdd,zeroMul], 0*b+0+a, 0+0+a).
test(cas_rules) :- apply_rule_set([zeroMul,zeroAdd], 0*b+0+a, 0+a).
test(cas_rules) :- apply_rule_set([zeroAdd,zeroMul], sin(0*a+b), sin(0+b)).

test(cas_rules) :- apply_rule_set_repeat([zeroAdd,zeroMul], 0*b+0+a, a).
test(cas_rules) :- apply_rule_set_repeat([zeroMul,zeroAdd], 0*b+0+a, a).
test(cas_rules) :- apply_rule_set_repeat([zeroAdd,zeroMul], 1+1, 1+1).
test(cas_rules) :- apply_rule_set_repeat([oneMul], 1*b+a, b+a).
test(cas_rules) :- apply_rule_set_repeat([oneMul], 1*1*b+0+a, b+0+a).
test(cas_rules) :- apply_rule_set_repeat([zeroAdd,oneMul], 1*1*b+0+a, b+a).
test(cas_rules, fail) :- apply_rule_set_repeat([zeroAdd,zeroMul], 1+1, 2).
test(cas_rules) :- idRules(X), apply_rule_set_repeat(X, a^0+(0+b)+(1*c),1+b+c).

:- end_tests(cas_rules).
