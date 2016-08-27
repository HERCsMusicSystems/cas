% An individual rule based approach to expression manipulation.
%
%  This is the top level file.
%
% Chris Sangwin, August 2016.
%

:- 
[
'cas_rules.pl',
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

sublist(_, [], _, []).
sublist(Sub, [Term | Terms], Sub1, [Term1 |Terms1]) :-
  substitute(Sub, Term, Sub1, Term1),
  sublist(Sub, Terms, Sub1, Terms1).

