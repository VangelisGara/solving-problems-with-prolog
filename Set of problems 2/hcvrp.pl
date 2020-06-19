:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).

vehicles([35, 40, 55, 15, 45, 25, 85, 55]).

clients([c(15,  77,  97), c(23, -28,  64), c(14,  77, -39),
         c(13,  32,  33), c(18,  32,   8), c(18, -42,  92),
         c(19,  -8,  -3), c(10,   7,  14), c(18,  82, -17),
         c(20, -48, -13), c(15,  53,  82), c(19,  39, -27),
         c(17, -48, -13), c(12,  53,  82), c(11,  39, -27),
         c(15, -48, -13), c(25,  53,  82), c(14, -39,   7),
         c(22,  17,   8), c(23, -38,  -7)]).

/* 1.Each vehicle we are going to use is enumarated */
/* 2.Each client we will serve is enumarated */
/* 3.For each vehicle we create available positions for the client (0:ghost client = no client ) */
/* 4.Constraint client so that they exist in one vehicle */
/* 5.The positions taken by clients in the vehicle can't exceed the max weight */
/* 6.Get all possible combinations of these client's positions in the tracks */
/* 7.Calculate the cost of these combination */ % ! Could not implement
hcvrp(NCl, NVe, Timeout, Solution, Cost, Time):-
	cputime(Start),
	vehicles(V),
	clients(C),
	/* Get the Vehicles we will use */
	get_N_elements_from_list(V, NVe, Vehicles),
	/* Get the clients that we will serve */
	get_N_elements_from_list(C, NCl, Clients1),
	/* Ghost client , helps to keep correspondence */
	append([c(0,  0,  0)], Clients1, Clients),
	/* Enumerates each client and vehicle */
	length(Clients,CL),
	length(Vehicles,VL),
	create_enumerated_list(ListVehicles, VL),
	create_enumerated_list(ListClients, CL),
	/* Gets info for each client , weight-position */
	get_weights(Clients, W),
	/* Define Variables */
	Variables_length is NCl * NVe,
	length(Domains, Variables_length),
	/* Define Domains */
	Domains :: ListClients,
	NCl1 is NCl + 1,
	/* Apply Constraints */
	singularity_constraint(Domains, NCl1),
	vehicle_availability(Domains, NCl, Vehicles_Clients),
	weight_constraint(Vehicles_Clients, Vehicles, W),!,
	/* Search and provide solution */
	search(Domains, 0, input_order, indomain_middle, complete, []),
	vehicle_availability(Domains, NCl, Unformatted_Solution),
	format_solution(Unformatted_Solution, Solution),
	cputime(Stop),
	Time is Stop - Start,
	Cost = 'Could Not implement'.

/* ********** Constraints Predicate Definition ***************** */

/* Weight Constraint , sets the max weight for each vehicle used */
weight_constraint([], [], _).
weight_constraint([HC|TC], [HeadV|TailV], W):-
	weight_constraint(TC, TailV, W),
	weight_check(HC, W, Total_weight),
	sumlist(Total_weight, T),
	HeadV #>= T.

weight_check([], _, []).
weight_check([H|T], W, Total_weight):-
	weight_check(T, W, Total_weight1),
	element(H, W, Weight),
	add_first(Weight, Total_weight1, Total_weight).

/* Clients can't exist in more than 1 vehicle */
singularity_constraint(_, 1).
singularity_constraint(Solution, NCl):-
	NCl1 is NCl - 1,
	singularity_constraint(Solution, NCl1),
	atleast(1, Solution, NCl),
	atmost(1, Solution, NCl).

/* ******************** Secondary Predicates ************************ */

/* Get weights limit from vehicles used */
get_weights([], []).
get_weights([HC|TC], Clients_weights):-
	get_weights(TC, Clients_weights1),
	HC = c(Weight, _ , _ ),
	add_first(Weight, Clients_weights1, Clients_weights).

/* Format the solution search provides to the one asked */
format_solution([],[]).
format_solution([H|T],Domains):-
	format_solution(T,Unformatted_Solution),
	help_format(H,S2),
	delete_element(0,S2,S3),
	add_first(S3, Unformatted_Solution, Domains).

help_format( [] , [] ) .
help_format( [X|Xs] , [Y|Ys] ) :-
  ( number(X) -> Y is X-1 ; Y = X ) ,
  help_format(Xs,Ys).

/* For each vehicle , create available positions for the clients */
vehicle_availability([], _, []).
vehicle_availability(Solution, NCl, Vehicles_Clients):-
	get_clients_of_vehicle(Solution, NCl, NCl, LVehicle),
	append(LVehicle, T, Solution),
	vehicle_availability(T, NCl, ClofVehicles1),
	add_first(LVehicle, ClofVehicles1, Vehicles_Clients).

/* Get vehicle's clients */
get_clients_of_vehicle(_, NCl, 0, []).
get_clients_of_vehicle([H|T], NCl, C, LVehicle):-
	C1 is C - 1,
	get_clients_of_vehicle(T, NCl, C1, LVehicle1),
	add_first(H, LVehicle1, LVehicle).

euclidean_distance(X1, Y1, X2, Y2, Distance) :- 
	D is sqrt((X2-X1)^2 + (Y2-Y1)^2),
	D1 is D * 1000,
	round(D1, Distance).
/* ******************** Helping Predicates ************************ */
get_N_elements_from_list(_, 0, []).
get_N_elements_from_list([HC|TC], N, Clients):-
	N1 is N - 1,
	get_N_elements_from_list(TC, N1, Clients1),
	add_first(HC, Clients1, Clients).

delete_element(X, [], []) :- !.
delete_element(X, [X|Xs], Y) :- !, delete_element(X, Xs, Y).
delete_element(X, [T|Xs], Y) :- !, delete_element(X, Xs, Y2), append([T], Y2, Y).

create_enumerated_list([], 0).
create_enumerated_list(Nlist, N):-
	N1 is N - 1,
	create_enumerated_list(Nlist1, N1),
	add_last(N, Nlist1, Nlist).

add_first(X, L, [X|L]).

add_last(X, [], [X]).
add_last(X, [Y|L1], [Y|L2]):- add_last(X,L1,L2).