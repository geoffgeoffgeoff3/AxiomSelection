%------------------------------------------------------------------------------
%----Dijkstra's algorithm, taken from Roman Bartak's page ...
%----    http://kti.ms.mff.cuni.cz/~bartak/prolog/graphs.html
%------------------------------------------------------------------------------
% dijkstra(+Graph,+Start,-ShortestPaths)
dijkstra(Graph,Start,ShortestPaths):-
   do_dijkstra(Graph,[],[Start-0],ShortestPaths).

%------------------------------------------------------------------------------
% dijkstra(+Graph,+ClosedVertices,+OpenVertices,-Distances)
do_dijkstra(_,ShortestPaths,[],ShortestPaths):-
    !.

do_dijkstra(Graph,Closed,Open,ShortestPaths):-
   choose_open_closest(Open,V-D,RestOpen),
   neighbourhood(Graph,V,Neighbours),  
   list_difference(Neighbours,Closed,OpenNeighbours),
   merge(OpenNeighbours,RestOpen,D,NewOpen),
   do_dijkstra(Graph,[V-D|Closed],NewOpen,ShortestPaths).

%------------------------------------------------------------------------------
%----A list of adjacent vertices+distance to V
neighbourhood(Graph,From,Neighbours):-
    findall(To-Cost,
        member(From-To-->Cost,Graph),
        RHSNeighbours),
    findall(To-Cost,
        member(To-From-->Cost,Graph),
        LHSNeighbours),
    append(RHSNeighbours,LHSNeighbours,Neighbours).

%------------------------------------------------------------------------------
% choose_v(+OpenVertices,-VertexToExpand,-RestOpenVertices)
choose_open_closest([H|T],MinV,Rest):-
   choose_minv(T,H,MinV,Rest).

choose_minv([],MinV,MinV,[]).

choose_minv([V1-D1|T],V-D,MinV,[H2|Rest]):-
   ( D1 < D 
   ->( NextM = (V1-D1),
       H2 = (V-D) )
   ; ( NextM = (V-D),
       H2 = (V1-D1) ) 
   ),
   choose_minv(T,NextM,MinV,Rest).

%------------------------------------------------------------------------------
%----list_difference(+ListOfVertices,+Closed,-ListOfNonClosedVertices)
list_difference([],_,[]).

list_difference([(V-D)|T],Closed,L):-
   ( member(V-_,Closed) 
   ->  L=NewT 
   ;   L=[(V-D)|NewT] 
   ),
   list_difference(T,Closed,NewT).

%------------------------------------------------------------------------------
% merge(+ListOfVertices,+OldOpenVertices,-AllOpenVertices)
merge([],L,_,L).

merge([V1-D1|T],Open,D,NewOpen):-
   ( remove(Open,V1-D2,RestOpen)
   ->  VD is min(D2,D+D1)
   ; ( RestOpen = Open,
       VD is D+D1 ) 
   ),
   NewOpen = [(V1-VD)|SubOpen],
   merge(T,RestOpen,D,SubOpen).

%------------------------------------------------------------------------------
remove([H|T],H,T):-
    !.

remove([H|T],X,[H|NT]):-
   remove(T,X,NT).

%------------------------------------------------------------------------------
