% Start to write individual rules.

module(cas_rules, [zeroAddp/1, map/3]).

% Each rule has a predicate ending in "p" and the function itself which performs the rule.
zeroAddp(0+_, true) :- ! .
zeroAddp(_, false).

zeroAdd(0+A, A) :- !.
zeroAdd(A, A).


% Apply a single rule.
apply(Rule, Expression, Result) :- Result =.. [Rule, Expression].

% Map an operator onto a list of expressions.
map(_, [], []) :- !.
map(R, [X | L], [X1 | L1]) :- 
    X1 =.. [R, X],
    map(R, L, L1).

% Map an operator onto a list of expressions.
map_rule(_, [], []) :- !.
map_rule(R, [X | L], [AX | L1]) :- 
    X1 =.. [R, X, AX],
    X1,                 % This line actually executes the function.
    map_rule(R, L, L1).

% apply_rule(zeroAdd, 3*(0+a), S)

% Recurse rule accross expression tree.
% cas_apply_rule(Rule, Expression, SimplifiedResult)
apply_rule(_, E, E) :- atomic(E), !.
%apply_rule(R, E, S) :-
%   E =.. [Op | Args],
%   MR =.. [apply_rule, R, _A1, _S1],
%   map_rule(MR, Args, Argsr),
%   S =.. [R, MR].
apply_rule(R, E, Argsr) :-
   E =.. [_Op | Args],

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

test(cas_rules) :- map_rule(zeroAdd, [], []).
test(cas_rules) :- map_rule(zeroAdd, [0+a], [a]).
test(cas_rules) :- map_rule(zeroAdd, [a+0], [a+0]).
test(cas_rules, fail) :- map_rule(zeroAdd, [0+a], [0+a]).
test(cas_rules) :- map_rule(zeroAdd, [0+a,b,0+sin(c)], [a,b,sin(c)]).

%test(cas_rules) :- apply_rule(zeroAdd, a, a).
%test(cas_rules) :- apply_rule(zeroAdd, 1, 1).
%test(cas_rules) :- apply_rule(zeroAdd, 0.5, 0.5).


:- end_tests(cas_rules).
