
%%%%%%%%% Simple Prolog Planner %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Based on one of the sample programs in:
%%%
%%% Artificial Intelligence:
%%% Structures and strategies for complex problem solving
%%%
%%% UCF FALL 2015
%%% AI CAP4630 Prolog assignment
%%%  Authors : Andrea Castillo and Kevin Anderson
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module( planner,
	   [
	       plan/6,change_state/3,conditions_met/2,member_state/2,
	       move/3,go/2,test/0,test2/0, test1/0
	   ]).

:- [utils].


plan(State, Goal, _, Moves, _,_) :-
   equal_set(State, Goal),
	write('moves are'), nl,
	reverse_print_stack(Moves).



plan(State, Goal, Been_list, Moves, Level, Sub_Level) :-
				Sub_Level =< Level,
				move(Name, Preconditions, Actions),
				conditions_met(Preconditions, State),
				change_state(State, Actions, Child_state),
				not(member_state(Child_state, Been_list)),
				stack(Child_state, Been_list, New_been_list),
				stack(Name, Moves, New_moves),
				NewSub_Level is Sub_Level+1,
			plan(Child_state, Goal, New_been_list, New_moves,Level,  NewSub_Level),!.

plan(State, Goal, Been_list, Moves, Level,Sub_Level) :-
New_Level is Level +1,
Sub_Level is 0,
 plan(State, Goal, Been_list, Moves, New_Level,Sub_Level).


change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
conditions_met(P, S) :- subset(P, S).

member_state(S, [H|_]) :-	equal_set(S, H).
member_state(S, [_|T]) :-	member_state(S, T).

/* move types */


move(pickup(X), [handempty, clear(X), room(1), on(X, Y)],
		[del(handempty), del(clear(X)), del(on(X, Y)),
				  add(clear(Y)),	add(holding(X))]).

move(pickup(X), [handempty, clear2(X), room(2), on2(X, Y)],
		[del(handempty), del(clear2(X)), del(on2(X, Y)),
				  add(clear2(Y)),	add(holding(X))]).

move(pickup(X), [handempty, room(1),  clear(X), ontable(X)],
		[del(handempty), del(clear(X)), del(ontable(X)),
				 add(holding(X))]).

move(pickup(X), [handempty, room(2), clear2(X), ontable2(X)],
		[del(handempty), del(clear2(X)), del(ontable2(X)),
				 add(room(2)),add(holding(X))]).

move(putdown(X), [holding(X), room(1)],
		[del(holding(X)), add(ontable(X)), add(clear(X)),
				  add(handempty)]).



move(putdown(X), [holding(X), room(2)],
		[del(holding(X)), add(ontable2(X)), add(clear2(X)),
				  add(handempty)]).

move(stack(X, Y), [holding(X), room(1), clear(Y)],
		[del(holding(X)), del(clear(Y)), add(handempty), add(on(X, Y)),
				  add(clear(X))]).

move(stack2(X, Y), [holding(X), room(2), clear2(Y)],
		[del(holding(X)), del(clear2(Y)), add(handempty), add(on2(X, Y)),
				  add(clear2(X))]).

move(goroom1, [room(2)],
		[add(room(1)),del(room(2))]).

move(goroom2, [room(1)],
		[add(room(2)),del(room(1))]).
/* run commands */

go(S, G) :- plan(S, G, [S], [], 0, 0).

test :- go([handempty, room(1), ontable(b), on(a,b),  clear(a)],
	          [handempty,  room(2), ontable2(b), on2(a,b), clear2(a)]).
test1:- go([handempty, room(1), ontable(a), on(b,a), on(c,b), on(d,c),  clear(d), ontable(e), clear(e)],
	          [handempty,  room(2),ontable2(a), on2(b,a), on2(c,b), on2(d,c),  clear2(d), ontable(e), clear(e)]).

test2:- go([handempty, room(1), ontable(a), on(b,a), clear(b), ontable(e), clear(e), ontable2(f), on2(g,f), clear2(g)],
	          [handempty, room(2), ontable2(a), on2(e,a), clear2(e), ontable(b), clear(b), ontable(f), on(g,f), clear(g)]).
