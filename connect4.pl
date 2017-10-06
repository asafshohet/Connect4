init_board([[],[],[],[],[],[], []]).

get_difficulty(X):-
	write('please choose difficulty level: normal, hard'),nl,read(Difficulty),
	(
		Difficulty == normal, !, X is 1;
		Difficulty == hard, !, X is 2;
		write('please choose correct difficulty level'),nl,get_difficulty(X)
	).

%%%%%%%%%% board validations %%%%%%%%%%

coloumn_len([],0).
coloumn_len([_|Xs], Length):-
	coloumn_len(Xs,NewLength), Length is NewLength+1.

coloumn_len([],_,0).
coloumn_len([Col|_], 1, Length):-
	!,coloumn_len(Col, Length).
coloumn_len([_|Columns], ColNum, Length):-
	NewColNum is ColNum-1, coloumn_len(Columns, NewColNum, Length).

column_full(Board, ColNum):-
	coloumn_len(Board, ColNum, Length),!, Length is 6.

board_full([]).
board_full([Col|Columns]):-
	column_full([Col],1), board_full(Columns).

%TODO
valid_move(Board, ColNum):-
	member(ColNum,[1,2,3,4,5,6,7]), \+ column_full(Board, ColNum).

%%%%%%%%%%%%%%%%%%%%%%% update board %%%%%%%%%%%%

update_board([],[],_,_,_).

update_board([Col|Columns], NewBoard, Turn, ColNumToUpdate, ColNumToUpdate):-
	!,NextColumn is ColNumToUpdate +1,
	update_board(Columns, TempBoard, Turn, ColNumToUpdate, NextColumn),
	append(Col, [Turn], NewCol),
	append([NewCol],TempBoard, NewBoard).

update_board([Col|Columns], NewBoard, Turn, ColNumToUpdate, CurrentColoumn):-
	!,NextColumn is CurrentColoumn +1,
	update_board(Columns, TempBoard, Turn, ColNumToUpdate, NextColumn),
	append([Col],TempBoard, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%% goal %%%%%%%%%%%%

is_color(_, 0,_):-!,false.
is_color([],_,_):-!,false.

is_color([X|_], 1, X).
is_color([_|Xs], Index, Turn):-
	NewIndex is Index-1, is_color(Xs, NewIndex, Turn).

is_color(_, 0, _, _):-!,false.
is_color(_, _,0, _):-!,false.
is_color([],_,_,_):-!,false.

is_color([Col|_], 1, Index, Turn):-
	is_color(Col, Index, Turn).

is_color([_|Columns], ColNum, Index, Turn):-
	NewNum is ColNum-1, is_color(Columns, NewNum, Index, Turn).

turn_diagonal_up_left(Board, ColNum, Index, Turn, Count):-
	NewColumn is ColNum-1, NewIndex is Index-1,
	(
		is_color(Board, NewColumn, NewIndex, Turn), !,
		turn_diagonal_up_left(Board, NewColumn, NewIndex, Turn, NewCount),
		Count is NewCount+1;

		Count is 0
	).

turn_diagonal_up_right(Board, ColNum, Index, Turn, Count):-
	NewColumn is ColNum+1, NewIndex is Index+1,
	(
		is_color(Board, NewColumn, NewIndex, Turn), !,
		turn_diagonal_up_right(Board, NewColumn, NewIndex, Turn, NewCount),
		Count is NewCount+1;

		Count is 0
	).
turn_diagonal_down_left(Board, ColNum, Index, Turn, Count):-
	NewColumn is ColNum-1, NewIndex is Index+1,
	(
		is_color(Board, NewColumn, NewIndex, Turn), !,
		turn_diagonal_down_left(Board, NewColumn, NewIndex, Turn, NewCount),
		Count is NewCount+1;

		Count is 0
	).
turn_diagonal_down_right(Board, ColNum, Index, Turn, Count):-
	NewColumn is ColNum+1, NewIndex is Index-1,
	(
		is_color(Board, NewColumn, NewIndex, Turn), !,
		turn_diagonal_down_right(Board, NewColumn, NewIndex, Turn, NewCount),
		Count is NewCount+1;

		Count is 0
	).

turn_row_right(Board, ColNum, Index, Turn, Count):-
	NewColumn is ColNum+1,
	(
		is_color(Board, NewColumn, Index, Turn), !,
		turn_row_right(Board, NewColumn, Index, Turn, NewCount),
		Count is NewCount+1;

		Count is 0
	).
turn_row_left(Board, ColNum, Index, Turn, Count):-
	NewColumn is ColNum-1,
	(
		is_color(Board, NewColumn, Index, Turn), !,
		turn_row_left(Board, NewColumn, Index, Turn, NewCount),
		Count is NewCount+1;

		Count is 0
	).

turn_col_down(Board, ColNum, Index, Turn, Count):-
	NewIndex is Index-1,
	(
		is_color(Board, ColNum, NewIndex, Turn), !,
		turn_col_down(Board, ColNum, NewIndex, Turn, NewCount),
		Count is NewCount+1;

		Count is 0
	).

goal(Board,ColNum, Index, Turn, AmountNeeded):-
	turn_col_down(Board, ColNum, Index, Turn, DownTurnCount), DownTurnCount >= AmountNeeded, !.
goal(Board,ColNum, Index, Turn, AmountNeeded):-
	turn_row_left(Board, ColNum, Index, Turn, LeftTurnCount),
	turn_row_right(Board, ColNum, Index, Turn, RightTurnCount), LeftTurnCount+RightTurnCount >= AmountNeeded, !.
goal(Board,ColNum, Index, Turn, AmountNeeded):-
	turn_diagonal_up_left(Board, ColNum, Index, Turn, LeftTurnCount),
	turn_diagonal_up_right(Board, ColNum, Index, Turn, RightTurnCount), LeftTurnCount+RightTurnCount >= AmountNeeded, !.
goal(Board,ColNum, Index, Turn, AmountNeeded):-
	turn_diagonal_down_left(Board, ColNum, Index, Turn, LeftTurnCount),
	turn_diagonal_down_right(Board, ColNum, Index, Turn, RightTurnCount), LeftTurnCount+RightTurnCount >= AmountNeeded, !.

goal(Board,ColNum, Turn):-
	coloumn_len(Board, ColNum, Index), goal(Board, ColNum, Index, Turn, 3).

%%%%%%%%%%%%%%%%%%% print board %%%%%%%

print_index([], _):- write('--').

print_index([X|_], 1):- write(X).
print_index([_|Xs], Index):-
	NewIndex is Index-1, print_index(Xs,NewIndex).

print_row([],_).
print_row([Col|Columns], LineNum):-
	print_index(Col, LineNum),tab(4), print_row(Columns, LineNum).

print_board(_,0).
print_board(Board,LineNum):-
	print_row(Board, LineNum),nl,
	NewLineNum is LineNum-1,
	print_board(Board, NewLineNum).

print_column_numbers([], _).
print_column_numbers([_|Columns], Count):-
	write(Count),tab(4), NewCount is Count+1, print_column_numbers(Columns, NewCount).

print_board(Board):-
	print_column_numbers(Board,1),nl,nl,
	print_board(Board, 6),nl.


%%%%%%%%%%%%%% entry point %%%%%%%%%%%%

switch_turn(x, o).
switch_turn(o, x).

update_and_play(Board, Turn, ColNum):-
	update_board(Board, NewBoard, Turn, ColNum, 1),print_board(NewBoard),nl,
	(
		goal(NewBoard, ColNum, Turn),!, write(Turn), write(' has prevaled! Game Over!'), nl;
		switch_turn(Turn,NewTurn), play(NewBoard,NewTurn)
	).

play(Board, _):-
	board_full(Board), !, write('Game Over! It is a Tie'), nl.

%user plays
play(Board, o):-
	write('o: select column (1-7)'),nl,read(ColNum),
	(
		integer(ColNum), !, valid_move(Board, ColNum), update_and_play(Board, o, ColNum);
		write('column '), write(ColNum), write(' is full or invalid. Please select another one!'), nl, nl, play(Board,o)
	).

% computer plays
play(Board, x):-
	alphabeta(o-_-Board,-1000,1000,_-ColNum-_,_,1),
	update_and_play(Board, x, ColNum).

play:- init_board(Board),print_board(Board), !, play(Board, x).
	%get_difficulty(Difficulty),asserta(dif(Difficulty)),



%%%%%%%%%%%%%% alpha-beta %%%%%%%%%%%%
% Pos is Turn-ColNum-NewBoard. Turn is the Turn played. ColNum is the column played. NewBoard is the board created this play
:- dynamic
	win/2. % win(Turn,Depth)

%x is computer. the max_to_move is opposite, since Turn is the turn played. not the next turn
max_to_move(o-_-_).
min_to_move(x-_-_).


save_if_win(Turn-ColNum-NewBoard, Depth, DidWeJustWin):-
	win(Turn,_), !;
	goal(NewBoard,ColNum,Turn),!, DidWeJustWin is 1, asserta(win(Turn,Depth)),!;
	true.

delete_if_win(Turn-_-_, DidWeJustWin):-
	nonvar(DidWeJustWin), !, retractall(win(Turn,_));
	true.

alphabeta(Pos,Alpha,Beta,GoodPos,Val,Depth):-
	Depth > 0, moves(Pos,PosList),!,
	boundedbest(PosList,Alpha,Beta,GoodPos,Val,Depth);
	staticval(Pos,Val).

%save_if_win(Pos,Depth,DidWeJustWin), %My addition
%delete_if_win(Pos,DidWeJustWin).%My addition
boundedbest([Pos|PosList],Alpha,Beta,GoodPos,GoodVal,Depth):-
	NewDepth is Depth - 1,
	alphabeta(Pos, Alpha,Beta,_, Val,NewDepth),
	goodenough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal,Depth).

goodenough([],_,_,Pos,Val,Pos,Val,_):- !.     % No other candidate

goodenough(_,Alpha,Beta,Pos,Val,Pos,Val,_):-
	min_to_move(Pos), Val > Beta,!;       % Maximizer attained upper bound
	max_to_move(Pos), Val < Alpha,!.      % Minimizer attained lower bound

goodenough(PosList,Alpha,Beta,Pos,Val,GoodPos,GoodVal,Depth):-
	newbounds(Alpha,Beta,Pos,Val,NewAlpha,NewBeta),        % Refine bounds
	boundedbest(PosList,NewAlpha,NewBeta,Pos1,Val1,Depth),
	betterof(Pos,Val,Pos1,Val1,GoodPos,GoodVal).

newbounds(Alpha,Beta,Pos,Val,Val,Beta):-
	min_to_move(Pos), Val > Alpha,!.        % Maximizer increased lower bound

newbounds(Alpha,Beta,Pos,Val,Alpha,Val):-
	max_to_move(Pos), Val < Beta,!.         % Minimizer decreased upper bound

newbounds(Alpha,Beta,_,_,Alpha,Beta).          % Otherwise bounds unchanged

betterof(Pos,Val,_,Val1,Pos,Val):-         % Pos better then Pos1
	min_to_move(Pos), Val > Val1,!;
	max_to_move(Pos), Val < Val1,!.

betterof(_,_,Pos1,Val1,Pos1,Val1).             % Otherwise Pos1 better



% list of all possible next states. template is Turn/ColNum/NewBoard
moves(Turn-_-Board, ListOfMoves):-
	switch_turn(Turn,NextTurn),  %switch the player
	findall(NextTurn-ColNum-NewBoard, (valid_move(Board, ColNum), update_board(Board, NewBoard, NextTurn, ColNum, 1)), ListOfMoves).

% heuristic functions %
amount_strait(Board, ColNum, Index, Turn, Val):-
	%goal(Board, ColNum, Index, Turn, 3), !, Val is 4;
	%goal(Board, ColNum, Index, Turn, 2), !, Val is 3;
	goal(Board, ColNum, Index, Turn, 1), !, Val is 2;
	Val is 0.


val_sign(Turn,Val,NewVal):-
	min_to_move(Turn-_-_), !, NewVal is Val;
	NewVal is -1*Val.

staticval(Turn-ColNum-Board, Val):-
	coloumn_len(Board, ColNum, Index),	amount_strait(Board, ColNum, Index, Turn, Res), val_sign(Turn,Res,Val).
	/*
	switch_turn(Turn,_),
	(
		% if other player won - give it the most negative value
		%win(OtherPlayer,WonDepth),!, Res is -7*WonDepth,val_sign(Turn,Res,Val);

		%if we won - give it high value (less than absolute value of 'OtherPlayer' winning
		%win(Turn,WonDepth), !, Res is 4*WonDepth, val_sign(Turn,Res,Val);

		% no one won so far - create static evaluation of new move

	).

*/




