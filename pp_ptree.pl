/*
 * file: pp_ptree.pl
 */

pp_ptree_list([]).
pp_ptree_list([H|T]) :-
  pp_ptree(H),
  pp_ptree_list(T).

pp_ptree(n(Name, Tree, none)) :- !,
  nl,
  write('('),
  pp_ptree_enter(1, Tree, 2),
  nl,
  write('  (ID '),
  write(Name),
  write('))'),
  nl.
pp_ptree(n(_, Tree, _)) :- !,
  pp_ptree_enter(0, Tree, 0),
  nl.
pp_ptree(Tree) :-
  pp_ptree_enter(0, Tree, 0),
  nl.

pp_ptree_enter(J, X, _) :-
  (
    atom(X)
  ;
    integer(X)
  ),
  !,
  (
    J == 0 ->
    true
  ;
    tab(1)
  ),
  write(X).
pp_ptree_enter(J, Tree, Column) :-
  (
    J == 1 ->
    tab(1)
  ;
    nl,
    tab(Column)
  ),
  functor(Tree, Label, N),
  write('('),
  write(Label),
  atom_length(Label, More),
  NextColumn is Column+More+2,
  pp_ptree_build(N, Tree, NextColumn).

pp_ptree_build(0, _, _) :- !, write(')').
pp_ptree_build(N, Tree, Column) :-
  arg(1, Tree, T),
  pp_ptree_enter(1, T, Column),
  N1 is N-1,
  pp_ptree_rest(N1, 2, Tree, Column).

pp_ptree_rest(0, _, _, _) :- !,
  write(')').
pp_ptree_rest(N, J, Tree, Column) :-
  arg(J, Tree, T),
  pp_ptree_enter(J, T, Column),
  N1 is N-1,
  J1 is J+1,
  pp_ptree_rest(N1, J1, Tree, Column).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if(current_prolog_flag(dialect, gprolog)).

run([]).
run([end_of_file]).
run([Tree|T]) :-
  pp_ptree(Tree),
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

