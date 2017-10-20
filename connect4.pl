/*
Programmer: Asaf Shohet
ID: 		305164816
Files Name: connect4.pl
Description: User vs Computer game of classic 'Connect 4'

Instructions: 
	winning the game is done by establishing 4 strait disks of your color.
	it can be in a line, in a row, or diagonaly.

Input:
	during the game, 'exit' will exit the program. 'restart' will restart the game.
	
Output:
	before each move, the program will print the current board status to the user

Synopsys:
	start the game by inputing 'play'.
	Follow instructions to choose first player, difficulty level, and board size.
	do your move by simply selecting the number of the column you wish to insert your disk into.
	computer is 'x'. you are 'o'.
	Good luck.
*/

%%%%%%%%%%%%%% entry point %%%%%%%%%%%%

play:- !,
	clear, 
	init_message, 
	set_difficuly, set_board_size,
	get_first_turn(Turn),
	lets_play_message,
	init_board(Board), print_board(Board),
	play(Board, Turn).

% before each play - verify board isn't full...
play(Board, _):-
	board_full(Board), !, write('Game Over! It is a Tie'), nl.

%user plays
play(Board, o):-
	write('o: select column (1-'),num_columns(NumColumns), write(NumColumns), write(')'), nl,read(ColNum),
	(
		% allow exit and restart in the middle of the game...
		ColNum == exit, !, write('Exiting game...'), nl;
		ColNum == restart, !, write('Restarting game...'), nl, nl, play;
		% validate input and play
		integer(ColNum), valid_move(Board, ColNum), !, update_and_play(Board, o, ColNum);
		write('column '), write(ColNum), write(' is full or invalid. Please select another one!'), nl, nl, play(Board,o)
	).

% computer plays
play(Board, x):-
	alphabeta(o-_-Board,-inf,inf,_-ColNum-_,_,4),
	write('X selected column '), write(ColNum),nl,nl,
	update_and_play(Board, x, ColNum).

% updates the board with the selected move, verify move hadn't won the game, and starts player's turn
update_and_play(Board, Turn, ColNum):-
	update_board(Board, NewBoard, Turn, ColNum, 1),print_board(NewBoard),nl,
	(
		goal(NewBoard, ColNum),!, write(Turn), write(' has prevaled! Game Over!'), nl;
		switch_turn(Turn,NewTurn), play(NewBoard,NewTurn)
	).


%%%%%%%%%%%%  init game utils %%%%%%%%%%%%%%%%%%%%%%%

:- dynamic
	num_columns/1, valid_columns/1, column_length/1, difficulty/1.

clear:-
	retractall(num_columns(_)),
	retractall(column_length(_)),
	retractall(difficulty(_)),
	retractall(valid_columns(_)).

% inits the board according to amount of columns
init_board([], 0).
init_board([[]|MoreColumns], NumColumns):-
	RemainedColumns is NumColumns - 1,
	init_board(MoreColumns, RemainedColumns).

init_board(Board):-
	num_columns(NumColumns),
	init_board(Board, NumColumns).

% switch turn to next player
switch_turn(x, o).
switch_turn(o, x).

error_message:-
	write('please select a valid option...'),nl.

init_message:-
	nl, write('Hello and Welcome to Connect4!'),nl,nl.

lets_play_message:-
	write('Let us play!'),nl, nl.
	
% ask the user to choose number of rows, and save the response
set_num_rows(NumRows):-
	integer(NumRows), NumRows =<9, NumRows>=4, asserta(column_length(NumRows)).
set_num_rows:-
	write('please select amount of rows, between 4 and 9:'),nl,read(NumRows),
	(
		set_num_rows(NumRows),!;
		error_message,set_num_rows
	).

% ask the user to choose number of columns, and save the response
set_num_columns(NumColumns):-
	integer(NumColumns), NumColumns =<9, NumColumns>=4, asserta(num_columns(NumColumns)),
		numlist(1,NumColumns,ValidColumns), asserta(valid_columns(ValidColumns)).
set_num_columns:-
	write('Please select amount of columns, between 4 and 9:'),nl,read(NumColumns),
	(
		set_num_columns(NumColumns), !;
		error_message,set_num_columns
	).

% improvment - dynamic board size. (allowed between 4-9 columns\rows, because out of this range it doesn't make a lot of sense...)
set_board_size:-
	write('Would you like to play with default (7x6) board size ? (select y or n)'),nl,read(Default),
	(
		Default == y, !, set_num_columns(7), set_num_rows(6);
		Default == n, !, set_num_columns, set_num_rows;
		error_message,set_board_size
	).

set_difficuly:-
	write('Please choose difficulty level: (select 1 for normal, 2 for hard)'),nl,read(Difficulty),
	(
		Difficulty == 1, !, asserta(difficulty(1));
		Difficulty == 2, !, asserta(difficulty(2));
		error_message,set_difficuly
	).

get_first_turn(Turn):-
	write('Would you like to start? (select y or n)'),nl,read(WantToStart),
	(
		WantToStart == y, !, Turn = o;
		WantToStart == n, !, Turn = x;
		error_message,get_first_turn(Turn)
	).

%%%%%%%%%% board validations %%%%%%%%%%

column_full(Board, ColNum):-
	col_info(Board, ColNum, Length, _),column_length(MaxLength), Length is MaxLength.

board_full([]).
board_full([Col|Columns]):-
	column_full([Col],1), board_full(Columns).

% validates (or generates) a move
valid_move(Board, ColNum):-
	valid_columns(ValidColumns), member(ColNum,ValidColumns), \+ column_full(Board, ColNum).

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

%%%%%%%%%%%%%%%%%%%%%%%%%% board info %%%%%%%%%%%%

% get color of index @Len in column @ColNum
% if Len is var, @Color is color of the highest disk in column, and @Len is the current height of the column

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


%%%%%%%%%%%%%%%%%%%%%%%%%% goal %%%%%%%%%%%%%%%%%%%

% True if the move played on @ColNum has won the game
goal(Board,ColNum):-
	col_info(Board, ColNum, Len,Color), goal(Board, ColNum, Len, Color, 3).

% True if @Board[@ColNum[@Index]] is part of a strait @AmountNeeded disks of color @Color in column @ColNum
goal(Board,ColNum, Index, Color, AmountNeeded):-
	turn_col_down(Board, ColNum, Index, Color, DownTurnCount), DownTurnCount >= AmountNeeded, !.

% True if @Board[@ColNum[@Index]] is part of a strait @AmountNeeded disks of color @Color in row @Index
goal(Board,ColNum, Index, Color, AmountNeeded):-
	turn_row_left(Board, ColNum, Index, Color, LeftTurnCount),
	turn_row_right(Board, ColNum, Index, Color, RightTurnCount), LeftTurnCount+RightTurnCount >= AmountNeeded, !.

% True if @Board[@ColNum[@Index]] is part of a strait @AmountNeeded disks of color @Color in diagonal up.
goal(Board,ColNum, Index, Color, AmountNeeded):-
	turn_diagonal_up_left(Board, ColNum, Index, Color, LeftTurnCount),
	turn_diagonal_up_right(Board, ColNum, Index, Color, RightTurnCount), LeftTurnCount+RightTurnCount >= AmountNeeded, !.
	
% True if @Board[@ColNum[@Index]] is part of a strait @AmountNeeded disks of color @Color in diagonal down.
goal(Board,ColNum, Index, Color, AmountNeeded):-
	turn_diagonal_down_left(Board, ColNum, Index, Color, LeftTurnCount),
	turn_diagonal_down_right(Board, ColNum, Index, Color, RightTurnCount), LeftTurnCount+RightTurnCount >= AmountNeeded, !.	


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


%%%%%%%%%%%%%%%%%%% print board %%%%%%%

print_board(Board):-
	print_column_numbers(Board,1),nl,nl,
	column_length(NumRows),
	print_board(Board, NumRows),nl.

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


%%%%%%%%%%%%%% alpha-beta %%%%%%%%%%%%

% Pos is Turn-ColNum-NewBoard. Turn is the Turn played. ColNum is the column played. NewBoard is the board created this play

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

%%%%%%% alpha beta missing functions %%%%%%

%x is computer. the max_to_move is opposite, since Turn is the turn played. not the next turn
max_to_move(o-_-_).
min_to_move(x-_-_).

% list of all possible next states. template is Turn-ColNum-NewBoard
moves(Turn-ColNum-Board, ListOfMoves):-
	!, 
	prev_move_no_win(Board, ColNum),	% fail if previous play is a win
	switch_turn(Turn,NextTurn),			% switch the player
	bagof(NextTurn-NextColNum-NewBoard, (valid_move(Board, NextColNum), update_board(Board, NewBoard, NextTurn, NextColNum, 1)), ListOfMoves).

% True if the move played on @ColNum didn't win the game
prev_move_no_win(_, ColNum):-
	var(ColNum),!. % possibly this is the first depth in alpha beta flow, thus ColNum might still be a var
prev_move_no_win(Board, ColNum):-
	\+ goal(Board, ColNum).

%%%%% heuristic function utils %%%%%%%

depth_factor(0,1):-!.
depth_factor(X,X).

two_val(1). 		% static val for 2 in disks strait
three_val(3).		% static val for 3 in disks strait
win_val(WinVal):-	% static val for 4 in disks strait (win)
	difficulty(Dif),win_val(WinVal, Dif).
	
win_val(WinVal,1):-
	!, WinVal is 7. % 'normal' difficulty. win for max might be lower than value of a couple of three in a row
win_val(WinVal,2):-
	% 'hard' difficulty. win for max must be the highest score possible
	num_columns(NumColumns), three_val(ThreeVal), WinVal is (ThreeVal*NumColumns)*2.

val_sign(Turn,Val,NewVal):-
	min_to_move(Turn-_-_), !, NewVal is Val;
	NewVal is -2*Val.
	
%%%%% heuristic function %%%%%%%

staticval(Turn-ColNum-Board, Val,Depth):-
	% victory value, if game was won
	goal(Board, ColNum), !, depth_factor(Depth,DepthFactor),
    win_val(WinVal), val_sign(Turn,WinVal,WinValSign), Val is WinValSign * DepthFactor;
	
	% no victory - estimate board
	board_val(Board, Board, Turn, 1, Val).
	
% static value of board %
board_val(_, [], _,_ , 0).
board_val(Board, [_|Cols], Turn, ColNum, Val):-
	column_val(Board, ColNum, ColumnRes),val_sign(Turn, ColumnRes, FinalColumnVal),
	NewColNum is ColNum+1,
	board_val(Board, Cols, Turn, NewColNum, BoardVal),
	Val is FinalColumnVal+BoardVal.

% static value of column %
column_val(Board, ColNum, Index, Turn, Val):-
	Index == 0, !, Val is 0;
	goal(Board, ColNum, Index, Turn, 2), !, three_val(Val);
	goal(Board, ColNum, Index, Turn, 1), !, two_val(Val);
	Val is 0.

column_val(Board, ColNum, Res):-
	col_info(Board, ColNum, Len, Color),
	column_val(Board, ColNum, Len, Color, Res).







