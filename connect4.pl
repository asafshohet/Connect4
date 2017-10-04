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


switch_turn(blue, yellow).
switch_turn(yellow, blue).


%%%%%%%%%%%%%%%update board%%%%%%%%

update_board([],[],_,_).

update_board([Col|Columns], NewBoard, Turn, ColNumToUpdate, ColNumToUpdate):-
	NextColumn is ColNumToUpdate +1, 
	update_board(Columns, TempBoard, Turn, ColNumToUpdate, NextColumn),
	append(Col, [Turn], NewCol), %add the new disk to column
	append(NewCol,TempBoard, NewBoard).

update_board([Col|Columns], NewBoard, Turn, ColNumToUpdate, CurrentColoumn):-
	NextColumn is CurrentColoumn +1, 
	update_board(Columns, TempBoard, Turn, ColNumToUpdate, NextColumn),
	append(Col,TempBoard, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal(_, _):-fail.

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
	write(Turn), write(': select column (1-7)'),nl,read(ColNum), %TODO - validate coloumn number
	(
		valid_move(Board, ColNum), !, update_and_play(Board, Turn, ColNum);
		write(ColNum), write(' is full... select another one'), play(Board,Turn)
	).
	
play:- empty_board(Board),play(Board, blue).
	%get_difficulty(Difficulty),assert(dif(Difficulty)),
	
	