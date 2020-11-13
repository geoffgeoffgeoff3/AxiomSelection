%------------------------------------------------------------------------------
%----Dijkstra's algorithm, taken from Roman Bartak's page ...
%----    http://kti.ms.mff.cuni.cz/~bartak/prolog/graphs.html
%------------------------------------------------------------------------------
% min_dist(+Graph,+Start,-MinDist)
min_dist(Graph,Start,MinDist):-
   dijkstra(Graph,[],[Start-0],MinDist).
   
%------------------------------------------------------------------------------
% dijkstra(+Graph,+ClosedVertices,+OpenVertices,-Distances)
dijkstra(_,MinDist,[],MinDist).
dijkstra(Graph,Closed,Open,MinDist):-
   choose_v(Open,V-D,RestOpen),
   neighbourhood(Graph,V,NB),  % NB is a list of adjacent vertices+distance to V
   diff(NB,Closed,NewNB),
   merge(NewNB,RestOpen,D,NewOpen),
   dijkstra(Graph,[V-D|Closed],NewOpen,MinDist).
   
%------------------------------------------------------------------------------
% choose_v(+OpenVertices,-VertexToExpand,-RestOpenVertices)
choose_v([H|T],MinV,Rest):-
   choose_minv(T,H,MinV,Rest).
choose_minv([],MinV,MinV,[]).
choose_minv([H|T],M,MinV,[H2|Rest]):-
   H=V1-D1, M=V-D,
   (D1<D -> NextM=H,H2=M
          ; NextM=M,H2=H),
   choose_minv(T,NextM,MinV,Rest).
   
%------------------------------------------------------------------------------
% diff(+ListOfVertices,+Closed,-ListOfNonClosedVertices)
diff([],_,[]).
diff([H|T],Closed,L):-
   H=V-D,
   (member(V-_,Closed) -> L=NewT ; L=[H|NewT]),
   diff(T,Closed,NewT).
   
%------------------------------------------------------------------------------
% merge(+ListOfVertices,+OldOpenVertices,-AllOpenVertices)
merge([],L,_,L).
merge([V1-D1|T],Open,D,NewOpen):-
   (remove(Open,V1-D2,RestOpen)
      -> VD is min(D2,D+D1)
       ; RestOpen=Open,VD is D+D1),
   NewOpen=[V1-VD|SubOpen],
   merge(T,RestOpen,D,SubOpen).
   
%------------------------------------------------------------------------------
remove([H|T],H,T).
remove([H|T],X,[H|NT]):-
   H\=X,
   remove(T,X,NT).

%------------------------------------------------------------------------------
