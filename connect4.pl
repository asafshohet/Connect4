empty_board([
	[],[],[],[],[],[], []]).

get_difficulty(X):-
	write('please choose difficulty level: normal, hard'),nl,read(Difficulty),
	(
		Difficulty == normal, !, X is 1;
		Difficulty == hard, !, X is 2;
		write('please choose correct difficulty level'),nl,get_difficulty(X)
	).

%%%%%%%%%% board validations %%%%%%%%%%

column_full([],Len):- !,Len == 0.
column_full([_|Xs],Len):-
	NewLen is Len-1, column_full(Xs,NewLen).

column_full(Col):-
	column_full(Col,6).

board_full([]).
board_full([Col|Rest]):-
	column_full(Col), board_full(Rest).


valid_move([], _, _).
valid_move([Col|_], ColNum, ColNum):- !, \+ column_full(Col).
valid_move([_|Columns], ColNum, CurrColNum):- NextColumn is CurrColNum+1, valid_move(Columns, ColNum, NextColumn).

valid_move(Board, ColNum):-valid_move(Board, ColNum, 1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


switch_turn(b, y).
switch_turn(y, b).


%%%%%%%%%%%%%%%update board%%%%%%%%

update_board([],[],_,_,_).

update_board([Col|Columns], NewBoard, Turn, ColNumToUpdate, ColNumToUpdate):-
	NextColumn is ColNumToUpdate +1,
	update_board(Columns, TempBoard, Turn, ColNumToUpdate, NextColumn),
	append(Col, [Turn], NewCol), %add the new disk to column
	append([NewCol],TempBoard, NewBoard).

update_board([Col|Columns], NewBoard, Turn, ColNumToUpdate, CurrentColoumn):-
	NextColumn is CurrentColoumn +1,
	update_board(Columns, TempBoard, Turn, ColNumToUpdate, NextColumn),
	append([Col],TempBoard, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal(_, _):-fail.

%%%%%%%%%%%%%%%%%%%print board%%%%%%%

print_index([], _):- write(' ').

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
	print_board(Board, 6),
	print_column_numbers(Board,1),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_and_play(Board, Turn, ColNum):-
	update_board(Board, NewBoard, Turn, ColNum, 1),
	(
		goal(NewBoard, ColNum),!, write(Turn), write(' has prevaled! Game Over!'), nl;
		switch_turn(Turn,NewTurn), play(NewBoard,NewTurn)
	).

play(Board, _):-
	board_full(Board), !, write('Game Over! It is a Tie'), nl.

%board isn't full
play(Board, Turn):-
	print_board(Board), write(Turn), write(': select column (1-7)'),nl,read(ColNum), %TODO - validate coloumn number
	(
		valid_move(Board, ColNum), !, update_and_play(Board, Turn, ColNum);
		write('column '), write(ColNum), write(' is full. Please select another one'), nl, play(Board,Turn)
	).

play:- empty_board(Board),play(Board, b).
	%get_difficulty(Difficulty),assert(dif(Difficulty)),


