init_board([[],[],[],[],[],[], []]).


:- dynamic
	depth/0.

set_depth:-
	write('please choose difficulty level: normal, hard'),nl,read(Difficulty),
	(
		Difficulty == normal, !, Depth is 4, asserta(depth(Depth));
		Difficulty == hard, !, Depth is 5, asserta(depth(Depth));
		write('please choose correct difficulty level'),nl,get_difficulty
	).

get_first_turn(Turn):-
	write('do you want to start? (select y or n)'),nl,read(WantToStart),
	(
		WantToStart == y, !, Turn = o;
		WantToStart == n, !, Turn = x;
		write('please select a valid option...'),nl,get_first_turn(Turn)
	).

%%%%%%%%%% board validations %%%%%%%%%%

column_full(Board, ColNum):-
	col_info(Board, ColNum, Length, _),!, Length is 6.

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

% get color of index @Len in column @ColNum
% if Len is var, return ed the highest disk info
%col_info(_, 0,_):-!,false.
col_info([],0,_):-!.
col_info([X], Len,Color):-
	!,Len = 1, Color = X.
col_info([X|_], Len, X):-
	integer(Len), Len =1.
col_info([_|Xs], Len, Color):-
	integer(Len), !, NewIndex is Len-1, col_info(Xs, NewIndex, Color);
	col_info(Xs, NewLen, Color), Len is NewLen+1.

col_info(_, _, Len, _):-integer(Len), Len=0, !,false.
col_info(_,0,_,_):-!,false.
col_info([],_,_,_):-!,false.

col_info([Col|_], 1, Len, Color):-
	col_info(Col, Len, Color).

col_info([_|Columns], ColNum, Len, Color):-
	NewNum is ColNum-1, col_info(Columns, NewNum, Len, Color).

turn_diagonal_up_left(Board, ColNum, Index, Color, Count):-
	NewColumn is ColNum-1, NewIndex is Index-1,
	(
		col_info(Board, NewColumn, NewIndex, Color), !,
		turn_diagonal_up_left(Board, NewColumn, NewIndex, Color, NewCount),
		Count is NewCount+1;

		Count is 0
	).

turn_diagonal_up_right(Board, ColNum, Index, Color, Count):-
	NewColumn is ColNum+1, NewIndex is Index+1,
	(
		col_info(Board, NewColumn, NewIndex, Color), !,
		turn_diagonal_up_right(Board, NewColumn, NewIndex, Color, NewCount),
		Count is NewCount+1;

		Count is 0
	).
turn_diagonal_down_left(Board, ColNum, Index, Color, Count):-
	NewColumn is ColNum-1, NewIndex is Index+1,
	(
		col_info(Board, NewColumn, NewIndex, Color), !,
		turn_diagonal_down_left(Board, NewColumn, NewIndex, Color, NewCount),
		Count is NewCount+1;

		Count is 0
	).
turn_diagonal_down_right(Board, ColNum, Index, Color, Count):-
	NewColumn is ColNum+1, NewIndex is Index-1,
	(
		col_info(Board, NewColumn, NewIndex, Color), !,
		turn_diagonal_down_right(Board, NewColumn, NewIndex, Color, NewCount),
		Count is NewCount+1;

		Count is 0
	).

turn_row_right(Board, ColNum, Index, Color, Count):-
	NewColumn is ColNum+1,
	(
		col_info(Board, NewColumn, Index, Color), !,
		turn_row_right(Board, NewColumn, Index, Color, NewCount),
		Count is NewCount+1;

		Count is 0
	).
turn_row_left(Board, ColNum, Index, Color, Count):-
	NewColumn is ColNum-1,
	(
		col_info(Board, NewColumn, Index, Color), !,
		turn_row_left(Board, NewColumn, Index, Color, NewCount),
		Count is NewCount+1;

		Count is 0
	).

turn_col_down(Board, ColNum, Index, Color, Count):-
	NewIndex is Index-1,
	(
		col_info(Board, ColNum, NewIndex, Color), !,
		turn_col_down(Board, ColNum, NewIndex, Color, NewCount),
		Count is NewCount+1;

		Count is 0
	).

goal(Board,ColNum, Index, Color, AmountNeeded):-
	turn_col_down(Board, ColNum, Index, Color, DownTurnCount), DownTurnCount >= AmountNeeded, !.
goal(Board,ColNum, Index, Color, AmountNeeded):-
	turn_row_left(Board, ColNum, Index, Color, LeftTurnCount),
	turn_row_right(Board, ColNum, Index, Color, RightTurnCount), LeftTurnCount+RightTurnCount >= AmountNeeded, !.
goal(Board,ColNum, Index, Color, AmountNeeded):-
	turn_diagonal_up_left(Board, ColNum, Index, Color, LeftTurnCount),
	turn_diagonal_up_right(Board, ColNum, Index, Color, RightTurnCount), LeftTurnCount+RightTurnCount >= AmountNeeded, !.
goal(Board,ColNum, Index, Color, AmountNeeded):-
	turn_diagonal_down_left(Board, ColNum, Index, Color, LeftTurnCount),
	turn_diagonal_down_right(Board, ColNum, Index, Color, RightTurnCount), LeftTurnCount+RightTurnCount >= AmountNeeded, !.

goal(Board,ColNum):-
	col_info(Board, ColNum, Len,Color), goal(Board, ColNum, Len, Color, 3).

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
		goal(NewBoard, ColNum),!, write(Turn), write(' has prevaled! Game Over!'), nl;
		switch_turn(Turn,NewTurn), play(NewBoard,NewTurn)
	).

play(Board, _):-
	board_full(Board), !, write('Game Over! It is a Tie'), nl.

%user plays
play(Board, o):-
	write('o: select column (1-7)'),nl,read(ColNum),
	(
		integer(ColNum), valid_move(Board, ColNum), !, update_and_play(Board, o, ColNum);
		write('column '), write(ColNum), write(' is full or invalid. Please select another one!'), nl, nl, play(Board,o)
	).

% computer plays
play(Board, x):-
	depth(Depth),
	alphabeta(o-_-Board,-1000,1000,_-ColNum-_,_,Depth),
	write('x selected column '), write(ColNum),nl,nl,
	update_and_play(Board, x, ColNum).

play:- !,init_board(Board),
	get_first_turn(Turn), asserta(depth(4)),
	print_board(Board), play(Board, Turn).



%%%%%%%%%%%%%% alpha-beta %%%%%%%%%%%%
% Pos is Turn-ColNum-NewBoard. Turn is the Turn played. ColNum is the column played. NewBoard is the board created this play

%x is computer. the max_to_move is opposite, since Turn is the turn played. not the next turn
max_to_move(o-_-_).
min_to_move(x-_-_).

alphabeta(Pos,Alpha,Beta,GoodPos,Val,Depth):-
	Depth > 0, moves(Pos,PosList),!,
	boundedbest(PosList,Alpha,Beta,GoodPos,Val,Depth);
	staticval(Pos,Val,Depth).

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



prev_move_no_win(_, ColNum, _):-
	var(ColNum),!.
prev_move_no_win(Board, ColNum):-
	\+ goal(Board, ColNum).

% list of all possible next states. template is Turn-ColNum-NewBoard
moves(Turn-ColNum-Board, ListOfMoves):-
	!,  prev_move_no_win(Board, ColNum),	% fail if previous play is a win
	switch_turn(Turn,NextTurn),					% switch the player
	findall(NextTurn-NextColNum-NewBoard, (valid_move(Board, NextColNum), update_board(Board, NewBoard, NextTurn, NextColNum, 1)), ListOfMoves),
	\+ ListOfMoves == [].

val_sign(Turn,Val,NewVal):-
	min_to_move(Turn-_-_), !, NewVal is Val;
	NewVal is -2*Val.

win_val(8).
three_val(3).
two_val(1).

% heuristic functions %
amount_strait(Board, ColNum, Index, Turn, Val):-
	Index == 0, !, Val is 0;
	% goal(Board, ColNum, Index, Turn, 3), !, win_val(Val);
	goal(Board, ColNum, Index, Turn, 2), !, three_val(Val);
	goal(Board, ColNum, Index, Turn, 1), !, two_val(Val);
	Val is 0.

column_val(Board, ColNum, Res):-
	col_info(Board, ColNum, Len, Color),
	amount_strait(Board, ColNum, Len, Color, Res).

board_val(_, [], _,_ , 0).
board_val(Board, [_|Cols], Turn, ColNum, Val):-
	column_val(Board, ColNum, ColumnRes),val_sign(Turn, ColumnRes, FinalColumnVal),
	NewColNum is ColNum+1,
	board_val(Board, Cols, Turn, NewColNum, BoardVal),
	Val is FinalColumnVal+BoardVal.


depth_factor(0,1):-!.
depth_factor(X,X).

staticval(Turn-ColNum-Board, Val,Depth):-
	(
	% test victory...
	depth_factor(Depth,DepthFactor),
	goal(Board, ColNum), !, win_val(Res), val_sign(Turn,Res,FinalRes), Val is FinalRes* DepthFactor;

	% estimate board
	board_val(Board, Board, Turn, 1, Val)
	).





