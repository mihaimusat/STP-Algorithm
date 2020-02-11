% Musat Mihai-Robert 
% Grupa 323CB

% Concateneaza 2 liste
append([], Rez, Rez) :- !.
append([H|T], Dr, [H|Rez]) :- append(T, Dr, Rez).

% Inverseaza o lista
rev([], Acc, Acc) :- !.
rev([Head|Tail], Acc, R) :- rev(Tail, [Head|Acc], R).

reverse(L, R) :- rev(L, [], R).

% Scoate elementul precizat din lista
remove(_, [], []) :- !.
remove(E, [E], []) :- !.
remove(E, [H], [H]) :- E \= H, !.
remove(E, [E|T], T) :- !.
remove(E, [H|T], Rez) :- E \= H, remove(E, T, Rez2), Rez = [H|Rez2].

% Concatenare de liste
% Verifica existenta unui nod intr-o multime
contains(D, [H]) :- H = [D, _], !.
contains(D, [H|_]) :- H = [D, _], !.
contains(D, [H|T]) :- H = [Nod, _], D \= Nod, contains(D, T).

% Obtine nodurile din graf
getNodes([Nodes, _], Nodes).

% Obtine muchiile din graf
getEdges([_, Edges], Edges).

% Obtine prioritatea unui nod
getPr(S, [S, Pr], Pr) :- !.
getPr(S, [[S, Pr]|_], Pr) :- !.
getPr(S1, [[S2, _]|T], Rez) :- S1 \= S2, getPr(S1, T, Rez).

% Obtine lista de adiacdnta a unui nod
getAdj(_, _, _, [], NewEdges, NewEdges, Rez, Rez) :- !.
getAdj(S, P, Dist, [[S, D, C]], AccE, AccE, AccR, Rez) :-
	Cost is C + Dist,
	Rez = [[S, D, Cost, P]|AccR], !.
getAdj(S, P, Dist, [[D, S, C]], AccE, AccE, AccR, Rez) :-
	Cost is C + Dist,
	Rez = [[S, D, Cost, P]|AccR], !.
getAdj(S, _, _, [[S2, D, C]], AccE, [[S2, D, C]|AccE], AccR, AccR) :-
	S2 \= S, D \= S, !.
getAdj(S, P, Dist, [[S, D, C]|T], AccE, NewEdges, AccR, Rez) :-
	Cost is C + Dist,
	T \= [],
	getAdj(S, P, Dist, T, AccE, NewEdges, [[S, D, Cost, P]|AccR], Rez).
getAdj(S, P, Dist, [[D, S, C]|T], AccE, NewEdges, AccR, Rez) :-
	Cost is C + Dist,
	T \= [],
	getAdj(S, P, Dist, T, AccE, NewEdges, [[S, D, Cost, P]|AccR], Rez).
getAdj(S, P, Dist, [[S2, D, C]|T], AccE, NewEdges, AccR, Rez) :- 
	S2 \= S, D \= S, T \= [],
	getAdj(S, P, Dist, T, [[S2, D, C]|AccE], NewEdges, AccR, Rez).
	
% Gaseste parintele unui nod 
getParent(S, [[D, S]|_], D) :- !.
getParent(S, [[_, N]|T], Rez) :- N \= S, getParent(S, T, Rez).

% Gaseste calea de la radacina pana la nodul dorit
getPath(Root, Root, _, _, [Root]) :- !.
getPath(Src, Root, Edges, Acc, Rez) :- getParent(Src, Edges, D),
	D \= Root, getPath(D, Root, Edges, [D|Acc], Rez).
getPath(Src, Root, Edges, Acc, Rez) :- getParent(Src, Edges, Root),
	Rez = [Root|Acc], !.

% Obtine radacina din graf
getRoot([], _,[-1, -1]) :- !.
getRoot([[_,P]], [R1, R2], R) :- P >= R2, R = [R1, R2], !.
getRoot([[N,P]], [_, R2], R) :- P < R2, R = [N, P], !.
getRoot([[_,P]|Tail], [R1, R2] , R) :- P >= R2, getRoot(Tail, [R1, R2], R).
getRoot([[N,P]|Tail], [_, R2] , R) :- P < R2, getRoot(Tail, [N, P], R).

% Extragem radacina, initializand cu infinit valorea initiala a radacinii
root(L, R) :- getRoot(L, [1000000, 1000000], R).

% Implementare PriorityQueue de muchii
insOrdE(E, [], [E]) :- !.
insOrdE(E, [H|T], R) :- E = [_, _, C1, _], H = [_, _, C2, _],
						C1 < C2, R = [E,H|T], !.
insOrdE(E, [H|T], R) :- E = [_, _, C1, P1], H = [_, _, C2, P2],
						C1 =:= C2, P1 =< P2, R = [E,H|T], !.
insOrdE(E, [H|T], R) :- E = [_, _, C1, P1], H = [_, _, C2, P2],
						C1 =:= C2, P1 > P2, insOrdE(E, T, Rez), R = [H|Rez].
insOrdE(E, [H|T], R) :- E = [_, _, C1, _], H = [_, _, C2, _],
						C1 > C2, insOrdE(E, T, Rez), R = [H|Rez].

order([], Rez, Rez) :- !.
order([H|T], Acc, Rez) :- insOrdE(H, Acc, Aux), order(T, Aux, Rez).

% Insereaza in coada initiala muchiile adiacente unui nod
insertQ(Init, [SrcN, SrcP], Dist, Nodes, Edges, NewNodes, NewEdges, Rez) :-
	getAdj(SrcN, SrcP, Dist, Edges, [], NewEdges, [], Aux),
	remove([SrcN, SrcP], Nodes, NewNodes),
	order(Aux, Init, Rez).

% Implementarea algoritmului lui Dijkstra
dijkstra([], _, _, Rez, Rez) :- !.
dijkstra(_, [], _, Rez, Rez) :- !.
dijkstra([H|T], Nodes, Edges, Acc, Rez) :-
	H = [_, D, _, _],
	not(contains(D, Nodes)),
	dijkstra(T, Nodes, Edges, Acc, Rez).
dijkstra([H|T], Nodes, Edges, Acc, Rez) :-
	H = [S, D, C, _],
	contains(D, Nodes),
	getPr(D, Nodes, P),
	insertQ(T, [D, P], C, Nodes, Edges, NewNodes, NewEdges, Pq),
	dijkstra(Pq, NewNodes, NewEdges, [[S, D]|Acc], Rez).
	
stp(Retea, Root, Edges) :- getEdges(Retea, Ed), getNodes(Retea, Nodes),
	root(Nodes, Root2), Root2 = [Root, _],
	insertQ([], Root2, 0, Nodes, Ed, NewNodes, NewEdges, Pq),
	dijkstra(Pq, NewNodes, NewEdges, [], Edges).

% Imbina cele 2 cai corespunzatoare celor doua noduri, eliminand duplicatele
merge(_, [], Rez, Rez) :- !.
merge([], _, Rez, Rez) :- !.
merge([Rez], [Rez], [Rez], [Rez]) :- !.
merge([E1], [E2], Last, Rez) :- E1 \= E2, Rez = [E1, Last, E2], !.
merge([E], [E|T], _, [E|T]) :- !.
merge([E|T], [E], _, [E|T]) :- !.
merge([H|T1], [H|T2], _, Rez) :- merge(T1, T2, H, Rez).
merge([H1|T1], [H2|T2], Last, Rez) :-
	H1 \= H2, reverse([Last, H1|T1], St),
	append(St, [H2|T2], Rez), !.

% Verifica daca drumul rezultat porneste din sursa sau din destinatie
check([Src|T], Src, [Src|T]) :- !.
check([H|T], Src, Rez) :- H \= Src, reverse([H|T], Rez), !.

drum(Retea, Src, Dst, Root, Edges, Path) :-
	stp(Retea, Root, Edges),
	getPath(Src, Root, Edges, [Src], Rez1),
	getPath(Dst, Root, Edges, [Dst], Rez2),
	merge(Rez1, Rez2, Root, Rez3),
	check(Rez3, Src, Path).


