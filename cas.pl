% An individual rule based approach to expression manipulation.
%
%  This is the top level file.
%
% Chris Sangwin, August 2016.
%

% Prolog notes:
% (1) To find the data structure underlying an expression in prolog: writef('%d', [<expr>]).
%
% CAS notes:
% (1) Coding convention: rules in camelCase, e.g. unaryMinus, toplevel CAS functions lowercase with underscores.

:- 
[
'cas_rules.pl',
'equal_exact.pl',
'equal_com_ass.pl'
].

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

sublist(_, [], _, []) :- !.
sublist(Sub, [Term | Terms], Sub1, [Term1 |Terms1]) :-
  substitute(Sub, Term, Sub1, Term1),
  sublist(Sub, Terms, Sub1, Terms1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(cas).

test(cas) :- substitute(x, 1+x^2, a, 1+a^2).

:- end_tests(cas).

