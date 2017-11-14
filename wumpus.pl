
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
	visitedTiles/2.

/* Defining adjecent */

adjecent(X,Y,M,N) :-
	(X is M + 1, Y is N);
	(X is M - 1, Y is N);
	(X is M, Y is N + 1);
	(X is M, Y is N - 1);


/* Rules about the stage - Can prolog know these? */

amount_weakEnemy(2).
amount_strongEnemy(2).
amount_pit(8).
amount_gold(3).
worldSize(12).
bulletDamage(20,50).
element(1,1).



/* Perceptions */
wall(X,Y) :- worldSize(S), (X > S; Y > S; X < 1; Y < 1).
smell(X,Y) :- enemy(M,N), adjecent(X,Y,M,N).
wind(X,Y) :- pit(M,N), adjecent(X,Y,M,N).
shine(X,Y) :- gold(X,Y).

/* Creating world */


randomizeElements :- 
	(worldSize(S),
		random_between(1,S,X), random_between(1,S,Y), not(element(X,Y)), assert(enemy(X,Y,100,50)), assert(element(X,Y)), write(X), write(" "), write(Y), nl,
		random_between(1,S,M), random_between(1,S,N), not(element(M,N)), assert(enemy(M,N,100,50)), assert(element(M,N)), write(M), write(" "), write(N), nl,
		random_between(1,S,O), random_between(1,S,P), not(element(O,P)), assert(enemy(O,P,100,20)), assert(element(O,P)), write(O), write(" "), write(P), nl,
		random_between(1,S,W), random_between(1,S,Z), not(element(W,Z)), assert(enemy(W,Z,100,20)), assert(element(W,Z)), write(W), write(" "), write(Z), nl,
		
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




init:- assert(currentPos(1,1)), assert(points(0)),assert(shots(5)),assert(currentLife(100)), assert(initialPos(1,1)).

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



	draw_horizontal(N,M):-
		
		(N > 0,
			(
				(
					not(element(N,M)),
					not(currentPos(N,M)),
					write("e"),
					C is N - 1,
					draw_horizontal(C,M)
				);
				(
					write("X"),
					C is N - 1,
					draw_horizontal(C,M)
				)			
			)
		);	
		nl,
		D is M - 1,
		worldSize(S),
		(
			D > 0,
			draw_horizontal(S,D)
		);
		true.

	draw_horizontal
		





