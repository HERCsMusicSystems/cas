% An individual rule based approach to expression manipulation.
%
% Chris Sangwin, August 2016.

module(cas_rules, [zeroAdd/2, map/3]).

% Each rule has a predicate ending in "p" and the function itself which performs the rule.
zeroAddp(0+_, true) :- ! .
zeroAddp(_, false).

zeroAdd(0+A, 0+A) :- !, fail.
zeroAdd(0+A, A) :- !.
zeroAdd(A, A).


zeroMulp(0*_, true) :- ! .
zeroMulp(_, false).

zeroMul(0*_A, 0) :- !.
zeroMul(A, A).


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
test(cas_rules) :- apply_rule(zeroAdd, 0.5, 0.5).

test(cas_rules) :- apply_rule(zeroAdd, 3*(0+a), 3*a).
test(cas_rules) :- apply_rule(zeroAdd, (0+b)*(0+a), b*a).
test(cas_rules) :- apply_rule(zeroAdd, (0+b)*(0+a)+7, b*a+7).

test(cas_rules) :- apply_rule(zeroMul, 0*(0+a), 0).

:- end_tests(cas_rules).
