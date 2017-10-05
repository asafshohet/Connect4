:- dynamic
        disk_count/2. % disk_count(Col,Count) - how much disks are in column @Col

init_board([[],[],[],[],[],[], []]):-
	retractall(disk_count(_,_)),
	asserta(disk_count(1,0)), 
	asserta(disk_count(2,0)), 
	asserta(disk_count(3,0)), 
	asserta(disk_count(4,0)), 
	asserta(disk_count(5,0)), 
	asserta(disk_count(6,0)), 
	asserta(disk_count(7,0)).

get_difficulty(X):-
	write('please choose difficulty level: normal, hard'),nl,read(Difficulty),
	(
		Difficulty == normal, !, X is 1;
		Difficulty == hard, !, X is 2;
		write('please choose correct difficulty level'),nl,get_difficulty(X)
	).

%%%%%%%%%% board validations %%%%%%%%%%
/*
column_full([],Len):- !,Len == 0.
column_full([_|Xs],Len):-
	NewLen is Len-1, column_full(Xs,NewLen).
*/
column_full(Col):-
	disk_count(Col,6).

board_full([], _).
board_full([_|Rest], ColNum):-
	column_full(ColNum), NextColNum is ColNum+1, board_full(Rest, NextColNum).

board_full(Board):-board_full(Board,1).
/*
valid_move([], _, _):-!,fail.
valid_move([Col|_], ColNum, ColNum):- !, \+ column_full(Col).
valid_move([_|Columns], ColNum, CurrColNum):- NextColumn is CurrColNum+1, valid_move(Columns, ColNum, NextColumn).
*/
valid_move(ColNum):-
	integer(ColNum), \+ column_full(ColNum).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


switch_turn(b, y).
switch_turn(y, b).


%%%%%%%%%%%%%%%update board%%%%%%%%

update_board([],[],_,_,_).

update_board([Col|Columns], NewBoard, Turn, ColNumToUpdate, ColNumToUpdate):-
	NextColumn is ColNumToUpdate +1,
	update_board(Columns, TempBoard, Turn, ColNumToUpdate, NextColumn),
	%update new disk in board and in metadata
	retract(disk_count(ColNumToUpdate,Count)), NewCount is Count+1, asserta(disk_count(ColNumToUpdate,NewCount)),
	append(Col, [Turn], NewCol), 
	append([NewCol],TempBoard, NewBoard).

update_board([Col|Columns], NewBoard, Turn, ColNumToUpdate, CurrentColoumn):-
	NextColumn is CurrentColoumn +1,
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
	
goal(Board,ColNum, Index, Turn):-
	turn_col_down(Board, ColNum, Index, Turn, DownTurnCount), DownTurnCount >= 3, !.
goal(Board,ColNum, Index, Turn):-	
	turn_row_left(Board, ColNum, Index, Turn, LeftTurnCount), 
	turn_row_right(Board, ColNum, Index, Turn, RightTurnCount), LeftTurnCount+RightTurnCount >= 3, !.
goal(Board,ColNum, Index, Turn):-	
	turn_diagonal_up_left(Board, ColNum, Index, Turn, LeftTurnCount), 
	turn_diagonal_up_right(Board, ColNum, Index, Turn, RightTurnCount), LeftTurnCount+RightTurnCount >= 3, !.
goal(Board,ColNum, Index, Turn):-	
	turn_diagonal_down_left(Board, ColNum, Index, Turn, LeftTurnCount), 
	turn_diagonal_down_right(Board, ColNum, Index, Turn, RightTurnCount), LeftTurnCount+RightTurnCount >= 3, !.
	
goal(Board,ColNum, Turn):-
	disk_count(ColNum,Index), goal(Board, ColNum, Index, Turn).	

%goal(_,_):-fail.
%%%%%%%%%%%%%%%%%%%print board%%%%%%%

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
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_and_play(Board, Turn, ColNum):-
	update_board(Board, NewBoard, Turn, ColNum, 1),
	(
		goal(NewBoard, ColNum, Turn),!, print_board(NewBoard), nl, write(Turn), write(' has prevaled! Game Over!'), nl;
		switch_turn(Turn,NewTurn), play(NewBoard,NewTurn)
	).

play(Board, _):-
	board_full(Board), !, write('Game Over! It is a Tie'), nl.

%board isn't full
play(Board, Turn):-
	print_board(Board), write(Turn), write(': select column (1-7)'),nl,read(ColNum), %TODO - validate coloumn number
	(
		valid_move(ColNum), !, update_and_play(Board, Turn, ColNum);
		write('column '), write(ColNum), write(' is full or invalid. Please select another one!'), nl, nl, play(Board,Turn)
	).

play:- init_board(Board),play(Board, b).
	%get_difficulty(Difficulty),asserta(dif(Difficulty)),


