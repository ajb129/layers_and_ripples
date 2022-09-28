
:- ['grammar.pl'].
:- ['interface.pl'].
:- ['pp_ptree.pl'].

:- import append/3 from basics.

parse(Phrase1) :-
  Phrase1 =.. L1,
  append(L1,[[X]-[]],L2),
  Phrase2 =.. L2,
  setof(X, tphrase(Phrase2), L),
  pp_ptree_list(L).
parse(Phrase1) :-
  Phrase1 =.. [H|L1],
  append([H|L1],[X-[]],L2),
  Phrase2 =.. L2,
  setof(_I, (tphrase(Phrase2), _I =.. [H|X]), _L),
  pp_ptree_list(_L).
parse(Phrase1) :-
  Phrase1 =.. L1,
  append(L1,[X],L2),
  Phrase2 =.. L2,
  setof(X, tphrase(Phrase2), L),
  pp_ptree_list(L).

