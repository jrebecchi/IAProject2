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
	possible_pit/3,
	possible_wumpus/3,
	scrumble/2,
	players_pit/2,
	players_wumpus/2,
	temp/2,
	sit/1.

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

/* Initializing funciton */


init:- 
	assert(currentPos(1,1)), 
	assert(points(0)),
	assert(shots(5)),
	assert(currentLife(100)), 
	assert(initialPos(1,1)),
	assert(direction(north)),
	assert(visitedTiles(1,1)),
	assert(scrumble(1,1)),
	randomizeElements,
	generate_pits(8),
	generate_golds(3),
	draw_horizontal(12,12).

/* Actions functions */

walk_foward :- 
	write("Player just walked!"), nl,update_score(-1),
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
					assert(visitedTiles(X,Z)),
					asserta(scrumble(X,Z)),
					apply_damage
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
					assert(visitedTiles(X,Z)),
					asserta(scrumble(X,Z)),
					apply_damage
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
					assert(visitedTiles(Z,Y)),
					asserta(scrumble(Z,Y)),
					apply_damage
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
					assert(visitedTiles(Z,Y)),
					asserta(scrumble(Z,Y)),
					apply_damage
				);
				turn_right
			)
		)
	).

	turn_right:-
		write("Player just turned right!"), nl,update_score(-1),
		direction(D),
		(
			(
				D = north,
				assert(direction(east)),
				retract(direction(north)),
				write("Player is facing east"), nl
			);
			(
				D = east,
				assert(direction(south)),
				retract(direction(east)),
				write("Player is facing south"), nl
			);
			(
				D = south,
				assert(direction(west)),
				retract(direction(south)),
				write("Player is facing west"), nl
			);
			(
				D = west,
				assert(direction(north)),
				retract(direction(west)),
				write("Player is facing north"), nl
			)
		).

	turn_left:-
		write("Player just turned left!"), nl,update_score(-1),
		direction(D),
		(
			(
				D = north,
				assert(direction(west)),
				retract(direction(north)),
				write("Player is facing west"), nl
			);
			(
				D = east,
				assert(direction(north)),
				retract(direction(east)),
				write("Player is facing north"), nl
			);
			(
				D = south,
				assert(direction(east)),
				retract(direction(south)),
				write("Player is facing east"), nl
			);
			(
				D = west,
				assert(direction(south)),
				retract(direction(west)),
				write("Player is facing south"), nl
			)
		).

	grab_object:-
		currentPos(X,Y),update_score(-1),
		gold(X,Y),
		update_score(1000),
		retract(gold(X,Y)),
		retract(element(X,Y)).
			/* Update score */
		

	climb_up:-
		currentPos(X,Y),update_score(-1),
		X is 1,
		Y is 1,
		retract(currentLife(ActualLife)),assert(currentLife(-1)),
		write("Player climb up outside the dungeon"),nl,
		worldSize(S).
		/* Update score */
		
	apply_damage:-
		(
			currentPos(XHero,YHero),
			enemy(XHero,YHero,_,Damage),
			currentLife(ActualLife),
			NewLife is ActualLife - Damage,
			retract(currentLife(ActualLife)),
			assert(currentLife(NewLife))
		);
		(
			currentPos(XHero,YHero),
			pit(XHero,YHero),
			currentLife(ActualLife),
			retract(currentLife(ActualLife)),
			assert(currentLife(0)),
			retract(currentPos(X,Y)),
			assert(currentPos(100,100)),
			update_score(-1000),
			write("The Player fell on the pit! G A M E O V E R"), nl,break

		);
		true.
			

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
		update_score(-10),
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
	/* basic movements for the decision_maker */
	movements:-
		(
			explore; 
			collect_scrumble;
			(X=1,Y=1;best_pathHome(X,Y)),gohome
		).

	/* The brain of the player */
	decision_maker :-
		currentPos(X,Y),
		(
			(
				not(gold(A,B)),(best_pathHome(X,Y),gohome)
			);
			(
				shine(X,Y)->grab_object
			);
			(

				(smell(X,Y),wind(X,Y))->(handle_smell,handle_breeze,movements);
				(smell(X,Y)->handle_smell)->(explore;(currentLife(HP),HP>50,points(CurrentPoints),CurrentPoints<1,move_risk);movements);
				((wind(X,Y)-> handle_breeze)->(explore;(points(CurrentPoints),CurrentPoints<1,-40<CurrentPoints,write("Risking!"),nl,explore_unsafe,
					points(NewPoints),-1000<NewPoints,currentPos(A,B),assert(safeTiles(A,B)));movements))
			);
			(
				safe_surroundings,write("Player feels safe!"), nl,movements

			);
			(
				collect_scrumble
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
			X1 is X+1,assert(possible_pit(X1,Y,east)),
			X2 is X-1,assert(possible_pit(X2,Y,west)),
			Y1 is Y+1,assert(possible_pit(X,Y1,north)),
			Y2 is Y-1,assert(possible_pit(X,Y2,south))
		).

	handle_smell:-
		currentPos(X,Y),(
			X1 is X+1,assert(possible_wumpus(X1,Y,east)),
			X2 is X-1,assert(possible_wumpus(X2,Y,west)),
			Y1 is Y+1,assert(possible_wumpus(X,Y1,north)),
			Y2 is Y-1,assert(possible_wumpus(X,Y2,south))
		).

	

	/* Safe! Exploring new places */

	explore:-
		currentPos(X,Y),
		(
			(M is X-1,M>0,safeTiles(M,Y),not(visitedTiles(M,Y)),direction(C),find_direction(C,west),walk_foward);
			(M is X+1,M<13,safeTiles(M,Y),not(visitedTiles(M,Y)),direction(C),find_direction(C,east),walk_foward);
			(M is Y-1,M>0,safeTiles(X,M),not(visitedTiles(X,M )),direction(C),find_direction(C,south),walk_foward);
			(M is Y+1,M<13,safeTiles(X,M),not(visitedTiles(X,M )),direction(C),find_direction(C,north),walk_foward)

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

		update_score(N):-
		points(S),
		X is S + N,
		retract(points(S)),
		assert(points(X)),
		write("The current score is "), write(X), nl.
	/* when there is any adjacent new safe tile he return to the old scrumble*/
	collect_scrumble:-
		scrumble(A,B),retract(scrumble(A,B)),scrumble(X,Y),retract(scrumble(X,Y)),(
			M is A-X,M<0,direction(C),find_direction(C,east),walk_foward;
			M is A-X,M>0,direction(C),find_direction(C,west),walk_foward;
			M is B-Y,M<0,direction(C),find_direction(C,north),walk_foward;
			M is B-Y,M>0,direction(C),find_direction(C,south),walk_foward
			).
	
	/* Main loop */
	run(init):-
		init,
		run(nextStep),
		true.
	
	run(nextStep):-
		sleep(0.5),nl,
		write("New Step"),nl,nl,
		
		(
			currentLife(LifeLevel),
			LifeLevel > 0,
			decision_maker,
			nextStepCaller,
			identify_wumpus(1,1),identify_pit(1,1)
		);
		(
			points(Score),
			format("------ Your final score is ~w", [Score]),nl
		),
		true.
		
	nextStepCaller:-
		run(nextStep),
		true.
	gohome:-
		currentPos(X,Y),1=X,1=Y,retract(currentLife(ActualLife)),assert(currentLife(0)),climb_up;
		going_home,worldSize(S),
		draw_horizontal(S,S),write("Going Home!"),nl,sleep(0.5),
		gohome.
	identify_wumpus(X,Y):-
	(
		(
		(possible_wumpus(X,Y,east);1=X),
		(possible_wumpus(X,Y,west);12=X),
		(possible_wumpus(X,Y,north);1=Y),
		(possible_wumpus(X,Y,south);12=Y),
		not(players_wumpus(X,Y)),
		assert(players_wumpus(X,Y))
		);true
	),
	(
		X<12,M is X+1,identify_wumpus(M,Y);
		Y<12,M is Y+1,identify_wumpus(X,M)
	);
	true.
	identify_pit(X,Y):-
	(	(
		(possible_pit(X,Y,east);1=X),
		(possible_pit(X,Y,west);12=X),
		(possible_pit(X,Y,north);1=Y),
		(possible_pit(X,Y,south);12=Y),
		not(players_pit(X,Y)),
		assert(players_pit(X,Y))
		);true
	),
	(
		X<12,M is X+1,identify_pit(M,Y);
		Y<12,M is Y+1,identify_pit(1,M);
		true
	).
draw_playersvision(N,M):-
		(N > 0,
			(
				write(" | "),
				(	
					worldSize(W2),
					InvertedN is W2 - N + 1, 
					currentPos(InvertedN,M),
					write("P"),
					C is N - 1,
					draw_playersvision(C,M)
				);
				(
					worldSize(W3),
					InvertedN is W3 - N + 1, 
					players_pit(InvertedN,M),
					write("O"),
					C is N - 1,
					draw_playersvision(C,M)
				);
				(
					worldSize(W4),
					InvertedN is W4 - N + 1, 
					players_wumpus(InvertedN,M),
					write("E"),
					C is N - 1,
					draw_playersvision(C,M)
				);
				(
					worldSize(W1),
					InvertedN is W1 - N + 1, 
					not(currentPos(InvertedN,M)),
					write(" "),
					C is N - 1,
					draw_playersvision(C,M)
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
			draw_playersvision(S,D)
		);
		true.
	playersvision:-
		worldSize(S),draw_playersvision(S,S).
	going_home:-
		temp(A,B),retract(temp(A,B)),temp(X,Y),(
			M is A-X,M<0,direction(C),find_direction(C,east),walk_foward;
			M is A-X,M>0,direction(C),find_direction(C,west),walk_foward;
			M is B-Y,M<0,direction(C),find_direction(C,north),walk_foward;
			M is B-Y,M>0,direction(C),find_direction(C,south),walk_foward
		).
	best_path(X,Y,A,B):-
	(
		X>A->false;
		Y>B->false;
		X=A,Y=B;
		M is X+1,safeTiles(M,Y),best_path(M,Y,A,B),assert(temp(M,Y));
		M is Y+1,safeTiles(X,M),best_path(X,M,A,B),assert(temp(X,M));
		false
	).
	best_pathHome(X,Y):-
	(
		best_path(1,1,X,Y),assertz(temp(1,1));
		collect_scrumble,worldSize(S),
		draw_horizontal(S,S),write("Going Home!"),nl,sleep(0.5),
		currentPos(A,B),best_pathHome(A,B)
	).
	move_risk:-

		write("risking"),nl,currentLife(ActualLife),explore_unsafe,(
			(currentLife(NewLife),ActualLife>NewLife,
			walk_foward,worldSize(S),draw_horizontal(S,S),write("Took damage"),nl,sleep(0.5),
			currentPos(X,Y),turn_right,turn_right,draw_horizontal(S,S),write("Gonna kill him!"),
			nl,sleep(0.5),shoot_scream,walk_foward);
			currentPos(X,Y),assert(safeTiles(X,Y)),write("Im Lucky"),nl
		).

	shoot_scream:-
		shoot,write("SHOOT!"),sleep(0.5),nl,(
			checa_enemies;
			shoot_scream
		).
	checa_enemies:-
	(
		enemy(X,Y,A,B),A<1,retract(enemy(X,Y,A,B)),retract(element(X,Y)),write("The wumpus screamed"),nl,
		worldSize(S),draw_horizontal(S,S),sleep(0.5);
		false
		).
	explore_unsafe:-
		currentPos(X,Y),
			(
				(M is X-1,M>1,not(visitedTiles(M,Y)),direction(C),find_direction(C,west),walk_foward);
				(M is X+1,M<12,not(visitedTiles(M,Y)),direction(C),find_direction(C,east),walk_foward);
				(M is Y-1,M>1,not(visitedTiles(X,M )),direction(C),find_direction(C,south),walk_foward);
				(M is Y+1,M<12,not(visitedTiles(X,M )),direction(C),find_direction(C,north),walk_foward)

		).