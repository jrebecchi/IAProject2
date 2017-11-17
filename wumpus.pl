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
	safeTiles/2,
	possible_pit/2,
	possible_wumpus/2.

/* Defining adjecent */

adjecent(X,Y,M,N) :-
	(X is M + 1, Y is N);
	(X is M - 1, Y is N);
	(X is M, Y is N + 1);
	(X is M, Y is N - 1).


/* Rules about the stage - Can prolog know these? */


amount_pit(8).
amount_gold(3).
worldSize(12).
bulletDamage(20,50).
element(1,1).



/* Perceptions */
wall(X,Y) :- worldSize(S), (X > S; Y > S; X < 1; Y < 1).
smell(X,Y) :- enemy(M,N,_,_), adjecent(X,Y,M,N) , write("Player smell enemy"),nl.
wind(X,Y) :- pit(M,N), adjecent(X,Y,M,N), write("Player feel a breeze"),nl.
shine(X,Y) :- gold(X,Y), write("Player is seeing a shiny thing"),nl.

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
	assert(visitedTiles(1,1)),
	randomizeElements,
	generate_pits(8),
	generate_golds(3),
	draw_horizontal(12,12).

/* Actions functions */

walk_foward :- 
	write("Player just walked!"), nl,
	currentPos(X,Y),
	direction(D),
	(
		(
			D = north,
			( 
				Z is Y + 1,
				(
					not(wall(X, Z)),
					assert(currentPos(X,Z)),
					retract(currentPos(X,Y)),
					assert(visitedTiles(X,Z))
				); 
				turn_right
			)
			
		);
		(
			D = south,
			( 
				Z is Y - 1,
				(
					not(wall(X, Z)),
					assert(currentPos(X,Z)),
					retract(currentPos(X,Y)),
					assert(visitedTiles(X,Z))
				);
				turn_right
			)
		);
		(
			D = east,
			( 
				Z is X + 1,
				(
					not(wall(Z, Y)),
					assert(currentPos(Z,Y)),
					retract(currentPos(X,Y)),
					assert(visitedTiles(Z,Y))
				);
				turn_right
			)
		);
		(
			D = west,
			( 
				Z is X - 1,
				(
					not(wall(Z, Y)),
					assert(currentPos(Z,Y)),
					retract(currentPos(X,Y)),
					assert(visitedTiles(Z,Y))
				);
				turn_right
			)
		)
	).

	turn_right:-
		write("Player just turned right!"), nl,
		direction(D),
		(
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
			)
		).

	turn_left:-
		write("Player just turned left!"), nl,
		direction(D),
		(
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
			)
		).

	grab_object:-
		currentPos(X,Y),
		gold(X,Y),
		retract(gold(X,Y)),
		retract(element(X,Y)).
			/* Update score */
		

	climb_up:-
		currentPos(X,Y),
		X is 1,
		Y is 1.
		/* Update score */

	apply_damage_to_enemies_column(X, [H|T], Damage):-
		Y is H,
		enemy(X,Y,Life,Damage_Power),
		New_Life is Life - Damage,
		retract(enemy(X,Y,Life,Damage_Power)),
		assert(enemy(X,Y,New_Life,Damage_Power)),
		apply_damage_to_enemies_column(X, T, Damage);
		apply_damage_to_enemies_column(X, T, Damage).
		
	apply_damage_to_enemies_line([H|T], Y, Damage):-
		X is H,
		enemy(X,Y,Life,Damage_Power),
		New_Life is Life - Damage,
		retract(enemy(X,Y,Life,Damage_Power)),
		assert(enemy(X,Y,New_Life,Damage_Power)),
		apply_damage_to_enemies_line(T, Y, Damage);
		apply_damage_to_enemies_line(T, Y, Damage).
		

	shoot:-
		(	
			bulletDamage(Min, Max),
			random_between(Min, Max, Damage),
			worldSize(Size),
			currentPos(M,N),
			direction(D),
			D = north,
			findall(Y, between(N,Size, Y), YList),
			apply_damage_to_enemies_column(M, YList, Damage)
		);
		(
			bulletDamage(Min, Max),
			random_between(Min, Max, Damage),
			worldSize(Size),
			currentPos(M,N),
			direction(D),
			D = east,
			findall(X, between(M,Size, X), XList),
			apply_damage_to_enemies_line(XList, N, Damage)
		);
		(
			bulletDamage(Min, Max),
			random_between(Min, Max, Damage),
			worldSize(Size),
			currentPos(M,N),
			currentPos(M,N),
			direction(D),
			D = south,	
			findall(Y, between(1,N, Y), YList),
			apply_damage_to_enemies_column(M, YList, Damage)
		);
		(
			bulletDamage(Min, Max),
			random_between(Min, Max, Damage),
			worldSize(Size),
			currentPos(M,N),
			currentPos(M,N),
			direction(D),
			D = west,
			findall(X, between(1,M, X), XList),
			apply_damage_to_enemies_line(XList, N, Damage)
		);
		true.


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
				shine(X,Y)->
				grab_object
			);
			(

				((smell(X,Y)->handle_smell),(wind(X,Y)-> handle_breeze))->find_safety;
				(smell(X,Y)->handle_smell)->find_safety;
				(wind(X,Y)-> handle_breeze)->find_safety
			);
			(
				safe_surroundings,
				write("Player feels safe!"), nl,
				explore /* function explore still not working - should go here */

			);
			(
				walk_foward
			)
		),
		worldSize(S),
		draw_horizontal(S,S).

	/* Turn around */

	go_back:-
		turn_right,
		turn_right,
		walk_foward.


	/* Mark surroundings as safe */

	safe_surroundings:-
		currentPos(X,Y),(
			X1 is X+1,assert(safeTiles(X1,Y)),
			X2 is X-1,assert(safeTiles(X2,Y)),
			Y1 is Y+1,assert(safeTiles(X,Y1)),
			Y2 is Y-1,assert(safeTiles(X,Y2))
		).

	/* Knowledge from danger */

	handle_breeze:-
		currentPos(X,Y),(
			X1 is X+1,assert(possible_pit(X1,Y)),
			X2 is X-1,assert(possible_pit(X2,Y)),
			Y1 is Y+1,assert(possible_pit(X,Y1)),
			Y2 is Y-1,assert(possible_pit(X,Y2))
		).
	handle_smell:-
		currentPos(X,Y),(
			X1 is X+1,assert(possible_wumpus(X1,Y)),
			X2 is X-1,assert(possible_wumpus(X2,Y)),
			Y1 is Y+1,assert(possible_wumpus(X,Y1)),
			Y2 is Y-1,assert(possible_wumpus(X,Y2))
		).
	/* Danger! Trying to find safe action */

	find_safety:-
		currentPos(X,Y),
		(
			(M is X+1,safeTiles(M,Y),direction(C),find_direction(C,east),walk_foward);
			(M is Y-1,safeTiles(X,M),direction(C),find_direction(C,south),walk_foward);
			(M is X-1,safeTiles(M,Y),direction(C),find_direction(C,west),walk_foward);
			(M is Y+1,safeTiles(X,M),direction(C),find_direction(C,north),walk_foward)
		).

	/* Safe! Exploring new places */

	explore:-
		write("Exploring!"),
		currentPos(X,Y),
		(
			(M is X-1,M>0,safeTiles(M,Y),not(visitedTiles(M,Y)),direction(C),find_direction(C,west),walk_foward);
			(M is X+1,M<13,safeTiles(M,Y),not(visitedTiles(M,Y)),direction(C),find_direction(C,east),walk_foward);
			(M is Y-1,M>0,safeTiles(X,M),not(visitedTiles(X,M )),direction(C),find_direction(C,south),walk_foward);
			(M is Y+1,M<13,safeTiles(X,M ),not(visitedTiles(X,M )),direction(C),find_direction(C,north),walk_foward)

		).

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