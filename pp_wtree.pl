
/*
 * file: pp_wtree.pl
 */

pp_wtree_list([]).
pp_wtree_list([H|T]) :-
  pp_wtree(H),
  pp_wtree_list(T).

pp_wtree(n(Name, Tree, none)) :- !,
  write('( '),
  pp_wtree_enter(Tree),
  write(' (ID '),
  write(Name),
  write('))'),
  nl.
pp_wtree(n(_, Tree, _)) :- !,
  pp_wtree_enter(Tree),
  nl.
pp_wtree(Tree) :-
  pp_wtree_enter(Tree),
  nl.

pp_wtree_enter(X) :-
  ( atom(X) ; integer(X) ), !,
  write(X).
pp_wtree_enter(Tree) :-
  functor(Tree, Label, N),
  write('('),
  write(Label),
  pp_wtree_build(N, Tree).

pp_wtree_build(0, _) :- !, write(')').
pp_wtree_build(N, Tree) :-
  arg(1, Tree, T),
  tab(1),
  pp_wtree_enter(T),
  N1 is N-1,
  pp_wtree_rest(N1, 2, Tree).

pp_wtree_rest(0, _, _) :- !, write(')').
pp_wtree_rest(N, J, Tree) :-
  arg(J, Tree, T),
  tab(1),
  pp_wtree_enter(T),
  N1 is N-1,
  J1 is J+1,
  pp_wtree_rest(N1, J1, Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if(current_prolog_flag(dialect, gprolog)).

run([]).
run([end_of_file]).
run([Tree|T]) :-
  pp_wtree(Tree),
  run(T).

:- initialization(main).

main:-
  input_to_list(user_input,Trees),
  run(Trees).

input_to_list(Stream,List) :-
  read(Stream,Term),
  read_list_from_stream(Term,Stream,List),
  close(Stream).

read_list_from_stream(end_of_file,_,[]) :-
  !.

read_list_from_stream(X,Stream,[X|Rest]) :-
  read(Stream,Term),
  read_list_from_stream(Term,Stream,Rest).

:- endif.

