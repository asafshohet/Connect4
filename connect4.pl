empty_board([
	[],[],[],[],[],[], []]).

get_difficulty(X):-
	write('please choose difficulty level: normal, hard'),nl,read(Difficulty),
	(	
		Difficulty == normal, !, X is 1;
		Difficulty == hard, !, X is 2;
		write('please choose correct difficulty level'),nl,get_difficulty(X)
	).
	
clear:-
	retractall(max_to_move(_)),
	retractall(min_to_move(_)),!.
	
play:-
	clear,
	assert(min_to_move(r/_)),assert(max_to_move(b/_)),
	get_difficulty(Difficulty),
	empty_board(Board).