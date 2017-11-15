
/* TODO - Add the score, add the shooting action, add player action feedback */


/* Dynamic variables */

:- dynamic
	enemy/4, 
	/* (X,Y,HP,DAMAGE) */
	pit/2,
	direction/1,
	initialPos/2,
	currentPos/2,
	gold/2,
	points/1,
	shots/1,
	currentLife/1,
	element/2,
	visitedTiles/2,
	possibleDanger/2.

/* Defining adjecent */

adjecent(X,Y,M,N) :-
	(X is M + 1, Y is N);
	(X is M - 1, Y is N);
	(X is M, Y is N + 1);
	(X is M, Y is N - 1);


/* Rules about the stage - Can prolog know these? */


amount_pit(8).
amount_gold(3).
worldSize(12).
bulletDamage(20,50).
element(1,1).



/* Perceptions */
wall(X,Y) :- worldSize(S), (X > S; Y > S; X < 1; Y < 1).
smell(X,Y) :- enemy(M,N,_,_), adjecent(X,Y,M,N).
wind(X,Y) :- pit(M,N), adjecent(X,Y,M,N).
shine(X,Y) :- gold(X,Y).

/* Creating world */


randomizeElements :- 
	(worldSize(S),
		random_between(1,S,X), random_between(1,S,Y), not(element(X,Y)), assert(enemy(X,Y,100,50)), assert(element(X,Y)), write(X), write(" "), write(Y), nl,
		random_between(1,S,M), random_between(1,S,N), not(element(M,N)), assert(enemy(M,N,100,50)), assert(element(M,N)), write(M), write(" "), write(N), nl,
		random_between(1,S,O), random_between(1,S,P), not(element(O,P)), assert(enemy(O,P,100,20)), assert(element(O,P)), write(O), write(" "), write(P), nl,
		random_between(1,S,W), random_between(1,S,Z), not(element(W,Z)), assert(enemy(W,Z,100,20)), assert(element(W,Z)), write(W), write(" "), write(Z), nl
		
	);
	abolish(element/2), assert(element(1,1)),randomizeElements.

generate_enemies(N):-
	N > 0,
	(
		(worldSize(S),
			random_between(1,S,X), random_between(1,S,Y), not(element(X,Y)), assert(enemy(X,Y,100,50)), assert(element(X,Y)), write(X), write(" "), write(Y), nl,
			C is N - 1,
			generate_enemies(C)
		);
		generate_enemies(N)
	);
	true.

/* Generation functions - Is it worth to do a recursive for enemies? */

generate_golds(N) :-
	N > 0,
	(
		(worldSize(S),
			random_between(1,S,X), random_between(1,S,Y),
			not(element(X,Y)), assert(gold(X,Y)), assert(element(X,Y)),
			write(X), write(" "), write(Y), nl,
			C is N - 1,
			generate_golds(C)			
		);
		generate_golds(N)
	);
	true.


generate_pits(N) :-
	N > 0,
	(
		(worldSize(S),
			random_between(1,S,X), random_between(1,S,Y),
			not(element(X,Y)), assert(pit(X,Y)), assert(element(X,Y)), 
			write(X), write(" "), write(Y), nl,
			C is N - 1,
			generate_pits(C)
		);
		generate_pits(N)
	);
	true.




init:- 
	assert(currentPos(1,1)), 
	assert(points(0)),
	assert(shots(5)),
	assert(currentLife(100)), 
	assert(initialPos(1,1)),
	assert(direction(north)),
	randomizeElements,
	generate_pits(8),
	generate_golds(3),
	draw_horizontal(12,12).

/* Actions functions */

walk_foward(D) :- 
	currentPos(X,Y),
	(
		(
			D = north, 
			Z is Y + 1,
			not(wall(X, Z)),
			assert(currentPos(X,Z)),
			retract(currentPos(X,Y)),
			assert(visitedTiles(X,Y)),
			write(X), write(Z)
		);
		(
			D = south, 
			Z is Y - 1,
			not(wall(X, Z)),
			assert(currentPos(X,Z)),
			retract(currentPos(X,Y)),
			assert(visitedTiles(X,Y)),
			write(X), write(Z)
		);
		(
			D = east, 
			Z is X + 1,
			not(wall(Z, Y)),
			assert(currentPos(Z,Y)),
			retract(currentPos(X,Y)),
			assert(visitedTiles(X,Y)),
			write(Z), write(Y)
		);
		(
			D = west, 
			Z is X - 1,
			not(wall(Z, Y)),
			assert(currentPos(Z,Y)),
			retract(currentPos(X,Y)),
			assert(visitedTiles(X,Y)),
			write(Z), write(Y)
		)
	).

	turn_right(D) :-
		(
			D = north,
			assert(direction(east)),
			retract(direction(north))
		);
		(
			D = east,
			assert(direction(south)),
			retract(direction(east))
		);
		(
			D = south,
			assert(direction(west)),
			retract(direction(south))
		);
		(
			D = west,
			assert(direction(north)),
			retract(direction(west))
		).

	turn_left(D) :-
		(
			D = north,
			assert(direction(west)),
			retract(direction(north))
		);
		(
			D = east,
			assert(direction(north)),
			retract(direction(east))
		);
		(
			D = south,
			assert(direction(east)),
			retract(direction(south))
		);
		(
			D = west,
			assert(direction(south)),
			retract(direction(west))
		).

	grab_object:-
		currentPos(X,Y),
		gold(X,Y),
		retract(gold(X,Y)).
			/* Update score */
		

	climb_up:-
		currentPos(X,Y),
		X is 1,
		Y is 1.
		/* Update score */

	shoot(D):-
		currentPos(M,N),
		(
			D = north,
			between(N,worldSize,X),
			enemy(M,X)
		);
		(
			D = east,
			between(M,worldSize,X),
			enemy(X,N)
		);
		(
			D = south,
			between(1,N,X),
			enemy(M,X)
		);
		(
			D = west,
			between(1,M,X),
			enemy(X,N)
		).


	/* Drawing the map - needs to be called each turn */

	draw_horizontal(N,M):-
		(N > 0,
			(
				write(" | "),
				(
					worldSize(W1),
					InvertedN is W1 - N + 1, 
					not(element(InvertedN,M)),
					not(currentPos(InvertedN,M)),
					write(" "),
					C is N - 1,
					draw_horizontal(C,M)
				);
				(	
					worldSize(W2),
					InvertedN is W2 - N + 1, 
					currentPos(InvertedN,M),
					write("P"),
					C is N - 1,
					draw_horizontal(C,M)
				);
				(
					worldSize(W3),
					InvertedN is W3 - N + 1, 
					pit(InvertedN,M),
					write("O"),
					C is N - 1,
					draw_horizontal(C,M)
				);
				(
					worldSize(W4),
					InvertedN is W4 - N + 1, 
					enemy(InvertedN,M,_,_),
					write("E"),
					C is N - 1,
					draw_horizontal(C,M)
				);
				(	
					worldSize(W5),
					InvertedN is W5 - N + 1, 
					gold(InvertedN,M),
					write("X"),
					C is N - 1,
					draw_horizontal(C,M)
				);
				(
					worldSize(W6),
					InvertedN is W6 - N + 1, 
					initialPos(InvertedN,M),
					write("S"),
					C is N - 1,
					draw_horizontal(C,M)
				)			
			)
		);
		write(" |"),nl,
		write("  ________________________________________________"),
		nl,
		D is M - 1,
		worldSize(S),
		(
			D > 0,
			draw_horizontal(S,D)
		);
		true.

	/* The brain of the player */

	decision_maker :-
		currentPos(X,Y),
		(
			(
				shine(X,Y),
				grab_object
			);
			(
				(smell(X,Y);wind(X,Y)),
				handle_danger(X,Y),
				find_safety(X,Y)
			);
			(
				direction(D),
				walk_foward(D)
			)
		),
		worldSize(S),
		draw_horizontal(S,S).


	/* Knowledge fron danger */

	handle_danger(X,Y):-
		(visitedTiles(M is X+1,Y);assert(possibleDanger(M is X+1, Y))),
		(visitedTiles(M is X-1,Y);assert(possibleDanger(M is X-1, Y))),
		(visitedTiles(X,M is Y+1);assert(possibleDanger(X, M is Y+1))),
		(visitedTiles(X,M is Y-1);assert(possibleDanger(X, M is Y-1))).

	/* Trying to find best action */

	find_safety(X,Y):-
		not(possibleDanger(M is X+1,Y)),direction(C),find_direction(C,east),walk_foward,
		not(possibleDanger(M is X-1,Y)),direction(C),find_direction(C,west),walk_foward,
		not(possibleDanger(X,M is Y+1)),direction(C),find_direction(C,north),walk_foward,
		not(possibleDanger(X,M is Y-1)),direction(C),find_direction(C,south),walk_foward.

	/* Finding the best turn to face the desired direction */

	find_direction(C,N):-
		(C = north,
			(
				N = north;
				(N = west, turn_left);
				(N = east, turn_right);
				(N = south, turn_right, turn_right)
			)
		);
		(C = south,
			(
				N = south;
				(N = east, turn_left);
				(N = west, turn_right);
				(N = north, turn_right, turn_right)
			)
		);
		(C = west,
			(
				N = west;
				(N = south, turn_left);
				(N = north, turn_right);
				(N = east, turn_right, turn_right)
			)
		);
		(C = east,
			(
				N = east;
				(N = north, turn_left);
				(N = south, turn_right);
				(N = west, turn_right, turn_right)
			)
		).

	










