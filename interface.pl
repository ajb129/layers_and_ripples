
:- ['./pp_wtree.pl'].

:- import append/3 from basics.

parse_tagged(Name, List) :-
  tphrase_set_string(List),
  setof(X, tphrase(utterance(Name, X)), L1),
  number_trees(0, L1, L2),
  pp_wtree_list(L2).

number_trees(_, [], []).
number_trees(N, [n(Name0, Tree, none)|Rest0], [n(Name, Tree, none)|Rest]) :-
  M is N+1,
  atom_concat(Name0, '_', Name1),
  number_chars(M, L),
  string:concat_atom(L, A),
  atom_concat(Name1, A, Name),
  number_trees(M, Rest0, Rest).

