%------------------------------------------------------------------------------
prolog_dialect(swi).                    %%swi
:-op(0,yfx,xor).                        %%swi
:-use_module(library(random)).          %%swi
:-use_module(library(lists)).          %%swi
tptp_directory('/home/tptp/TPTP').
%------------------------------------------------------------------------------
%----These are used in the TPTP and need to exist before the 
%----transformations and formats are loaded. They are also declared at 
%----runtime in tptp2X/4.
:-op(70,fx,'$$').
:-op(80,fx,'$').
:-op(85,fx,'&').    %----DLF dereference 
:-op(90,xfx,/).     %----Rationals need to bind tighter than =
:-op(100,fx,++).
:-op(100,fx,--).
%----Postfix for !=
:-op(100,xf,'!').
%---- .. used for range in tptp2X. Needs to be stronger than :
:-op(400,xfx,'..').
%----! and ? are of higher precedence than : so ![X]:p(X) is :(![X],p(X))
%----Otherwise ![X]:![Y]:p(X,Y) cannot be parsed.
%----! is fy so Prolog can read !! (blah) as !(!(blah)) and it gets fixed
:-op(400,fy,!).
:-op(400,fx,?).
:-op(400,fx,^).
:-op(400,fx,'!>').
:-op(400,fx,'?*').
:-op(400,fx,'@-').
:-op(400,fx,'@+').
:-op(401,fx,'@@-').
:-op(401,fx,'@@+').
:-op(402,fx,'!!').
:-op(402,fx,'??').
:-op(403,yfx,*).     %----X product
:-op(403,yfx,+).     %----Union
%----DLF ++ and ** and <+> and -= stronger than = and << and >>
:-op(403,yfx,'**').  %----DLF intersection
:-op(403,yfx,'++').  %----DLF union
:-op(403,yfx,'<+>'). %----DLF disjoint union
%----= must bind more tightly than : for ! [X] : a = X. = must binder looser
%----than quantifiers for otherwise X = ! [Y] : ... is a syntax error (the =
%----grabs the quantifier). That means for THF it is necessary to bracket 
%----formula terms, e.g., a = (! [X] : p(X))
:-op(405,xfx,'=').
%---!= not possible because ! is special - see postfix cheat :-op(405,xfx,'!=').
:-op(440,xfy,>).     %----Type arrow
%----Made @ stronger than : for TH1 type construction, e.g., rs: stream @ rule
%----Need : stronger than binary connectives for ! [X] : p(X) <=> !Y ...
%----Need ~ and : equal and right-assoc for ![X] : ~p and for ~![X] : ...
:-op(450,fy,~).      %----Logical negation
:-op(450,xfy,:).
:-op(501,yfx,@).
:-op(1102,xfy,'|').
:-op(1102,xfx,'~|').
:-op(1103,xfy,&).
:-op(1103,xfx,~&).
:-op(1104,xfx,=>).
:-op(1104,xfx,<=).
:-op(1105,xfx,<=>).
:-op(1105,xfx,<~>).
:-op(1110,xfx,-->).
%------------------------------------------------------------------------------
%----Runtime version of operators
declare_TPTP_operators:-
    op(70,fx,'$$'),
    op(80,fx,'$'),
    op(85,fx,'&'),
    op(90,xfx,/),
    op(100,fx,++),
    op(100,fx,--),
    op(100,xf,'!'),
    op(400,xfx,'..'),
    op(400,fy,!),
    op(400,fx,?),
    op(400,fx,^),
    op(400,fx,'!>'),
    op(400,fx,'?*'),
    op(400,fx,'@-'),
    op(400,fx,'@+'),
    op(400,fx,'@@-'),
    op(400,fx,'@@+'),
    op(402,fx,'!!'),
    op(402,fx,'??'),
    op(403,yfx,*),
    op(403,yfx,+),
    op(403,yfx,'**'),
    op(403,yfx,'++'),
    op(403,yfx,'<+>'),
    op(405,xfx,'='),
    op(440,xfy,>),
    op(450,fy,~),
    op(450,xfy,:),
    op(501,yfx,@),
    op(1102,xfy,'|'),
    op(1102,xfx,'~|'),
    op(1103,xfy,&),
    op(1103,xfx,~&),
    op(1104,xfx,=>),
    op(1104,xfx,<=),
    op(1105,xfx,<=>),
    op(1105,xfx,<~>),
    op(1110,xfx,-->),
    op(1125,xfx,#),
    op(1125,xf,#),
    op(1150,xfx,':=').
%------------------------------------------------------------------------------
:-consult('tptp2X.read').
:-consult('tptp2X.syntax').
:-consult('glgg-dijkstra.pl').
%------------------------------------------------------------------------------
%----Invert sign 
tptp2X_invert_sign(++,--).

tptp2X_invert_sign(--,++).
%------------------------------------------------------------------------------
%----Invert the sign of a TPTP literal
tptp2X_invert_literal(++Atom,--Atom).

tptp2X_invert_literal(--Atom,++Atom).
%------------------------------------------------------------------------------
%----Make a subset of a list
tptp2X_subset([],[]).

tptp2X_subset([_|T],Subset):-
    tptp2X_subset(T,Subset).

tptp2X_subset([H|T],[H|RestOfSubset]):-
    tptp2X_subset(T,RestOfSubset).
%------------------------------------------------------------------------------
%----Simple append, required in tptpread. Renamed to avoid confusion
%----with builtins.
tptp2X_append([],List,List).

tptp2X_append([Head|Tail],List,[Head|TailList]):-
    tptp2X_append(Tail,List,TailList).
%------------------------------------------------------------------------------
%----Simple select. Renamed to avoid confusion with builtins.
tptp2X_select(Element,[Element|Tail],Tail).

tptp2X_select(Element,[Head|Tail],[Head|SelectedTail]):-
    tptp2X_select(Element,Tail,SelectedTail).
%------------------------------------------------------------------------------
%----Select the Nth element.
tptp2X_select_Nth(Element,[Element|Tail],1,Tail).

tptp2X_select_Nth(Element,[Head|Tail],N,[Head|SelectedTail]):-
    integer(N),
    N > 1,
    NewN is N - 1,
    tptp2X_select_Nth(Element,Tail,NewN,SelectedTail).

tptp2X_select_Nth(Element,[Head|Tail],N,[Head|SelectedTail]):-
    var(N),
    tptp2X_select_Nth(Element,Tail,NewN,SelectedTail),
    N is NewN + 1.
%------------------------------------------------------------------------------
%----List difference
tptp2X_list_difference(Remainder,[],Remainder).

tptp2X_list_difference(List,[FirstToRemove|RestToRemove],Remainder):-
    tptp2X_select(FirstToRemove,List,OthersInList),
    !,
    tptp2X_list_difference(OthersInList,RestToRemove,Remainder).

tptp2X_list_difference(List,[_|RestToRemove],Remainder):-
    tptp2X_list_difference(List,RestToRemove,Remainder).
%------------------------------------------------------------------------------
%----Simple member. Renamed to avoid confusion with builtins.
tptp2X_member(Element,[Element|_]).

tptp2X_member(Element,[_|Tail]):-
    tptp2X_member(Element,Tail).
%------------------------------------------------------------------------------
%----Exact member without unification. Only a test, not for extraction
tptp2X_exact_member(Element,[Head|_]):-
    Element == Head.

tptp2X_exact_member(Element,[_|Tail]):-
    tptp2X_exact_member(Element,Tail).
%------------------------------------------------------------------------------
%----Simple length. Renamed to avoid confusion with builtins.
tptp2X_length([],0).

tptp2X_length([_|Tail],Length):-
    tptp2X_length(Tail,TailLength),
    Length is TailLength + 1.
%------------------------------------------------------------------------------
tptp2X_atom_length(Atom,AtomLength):-
     name(Atom,AtomASCII),
     tptp2X_length(AtomASCII,AtomLength).
%------------------------------------------------------------------------------
%----Setof1 does setof, but allows empty list to be returned
tptp2X_setof1(Variable,Goal,List):-
    setof(Variable,Goal,List),
    !.

tptp2X_setof1(_,_,[]).
%------------------------------------------------------------------------------
%----bagof1 does bagof, but allows empty list to be returned
tptp2X_bagof1(Variable,Goal,List):-
    bagof(Variable,Goal,List),
    !.

tptp2X_bagof1(_,_,[]).
%------------------------------------------------------------------------------
%----setof that ignores other variables and returns an empty list rather
%----than failing.
tptp2X_findall_setof1(Variable,Predicate,UniqueValues):-
    findall(Variable,Predicate,AllValues),
    tptp2X_setof1(Member,tptp2X_member(Member,AllValues),UniqueValues).
%------------------------------------------------------------------------------
%----Concatenate a list of terms into a big atom (only SWI here)
concatenate_atoms([Atom],Atom):-
    !.

concatenate_atoms([Atom|RestAtoms],ConcatenatedAtom):-
    concatenate_atoms(RestAtoms,RestAtom),
    atom_concat(Atom,RestAtom,ConcatenatedAtom).
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
weight(Var,_NestingLevel,1):-
    var(Var),
    !.

weight([],_,0):-
    !.

weight([Head|Rest],NestingLevel,Weight):-
    !,
    weight(Head,NestingLevel,HeadWeight),
    weight(Rest,NestingLevel,RestWeight),
    Weight is HeadWeight + RestWeight.

weight(Atom,0,3):-
    atom(Atom),
    !.
    
weight(Atom,NestingLevel,2):-
    atom(Atom),
    NestingLevel > 0,
    !.
    
weight(NonAtom,NestingLevel,Weight):-
    NonAtom =.. [Symbol|Terms],
    weight(Symbol,NestingLevel,SymbolWeight),
    NextNestingLevel is NestingLevel + 1,
    weight(Terms,NextNestingLevel,TermsWeight),
    Weight is SymbolWeight + TermsWeight.

%------------------------------------------------------------------------------
lgg_list([],[],SymbolIndex,SymbolIndex,_NestingLevel,[],0,0):-
    !.

lgg_list([Term1|Rest1],[Term2|Rest2],SymbolIndex,NextSymbolIndex,NestingLevel,
[LGG|LGGRest],Cost1,Cost2):-
    lgg_term(Term1,Term2,SymbolIndex,TermSymbolIndex,NestingLevel,
LGG,TermCost1,TermCost2),
    lgg_list(Rest1,Rest2,TermSymbolIndex,NextSymbolIndex,NestingLevel,
LGGRest,RestCost1,RestCost2),
    Cost1 is TermCost1 + RestCost1,
    Cost2 is TermCost2 + RestCost2.
%------------------------------------------------------------------------------
lgg_term(Term1,Term2,SymbolIndex,SymbolIndex,_NestingLevel,Term1,0,0):-
    Term1 == Term2,
    !.

lgg_term(Var1,Var2,SymbolIndex,SymbolIndex,NestingLevel,Var1,0,Weight2):-
    var(Var1),
    var(Var2),
    !,
    Var1 = Var2,
    weight(Var2,NestingLevel,Weight2).
    
lgg_term(Var,NonVar,SymbolIndex,SymbolIndex,NestingLevel,Var,0,Weight):-
    var(Var),
    nonvar(NonVar),
    !,
    weight(NonVar,NestingLevel,Weight).
    
lgg_term(NonVar,Var,SymbolIndex,SymbolIndex,NestingLevel,Var,Weight,0):-
    var(Var),
    nonvar(NonVar),
    !,
    weight(NonVar,NestingLevel,Weight).
    
%----Two constants -  variable LGG
lgg_term(Term1,Term2,SymbolIndex,SymbolIndex,NestingLevel,
_NewVar,Cost1,Cost2):-
   Term1 =.. [Term1],
   Term2 =.. [Term2],
   !,
   weight(Term1,NestingLevel,Cost1),
   weight(Term2,NestingLevel,Cost2).

%----Same principal symbols and aritys - cost of args
lgg_term(Term1,Term2,SymbolIndex,NextSymbolIndex,NestingLevel,
LGG,ArgsCost1,ArgsCost2):-
   Term1 =.. [Symbol|TermArgs1],
   Term2 =.. [Symbol|TermArgs2],
   length(TermArgs1,Arity),
   length(TermArgs2,Arity),
   !,
   NextNestingLevel is NestingLevel + 1,
   lgg_list(TermArgs1,TermArgs2,SymbolIndex,NextSymbolIndex,NextNestingLevel,
ArgsLGG,ArgsCost1,ArgsCost2),
   LGG =.. [Symbol|ArgsLGG].
    
lgg_term(Term1,Term2,SymbolIndex,NextSymbolIndex,NestingLevel,
LGG,Cost1,Cost2):-
   Term1 =.. [Symbol1|TermArgs1],
   Term2 =.. [Symbol2|TermArgs2],
   length(TermArgs1,Arity),
   length(TermArgs2,Arity),
   !,
   atom_concat('A',SymbolIndex,Symbol),
   ListSymbolIndex is SymbolIndex + 1,
   NextNestingLevel is NestingLevel + 1,
   lgg_list(TermArgs1,TermArgs2,ListSymbolIndex,NextSymbolIndex,
NextNestingLevel,ArgsLGG,ArgsCost1,ArgsCost2),
   weight(Symbol1,NestingLevel,SymbolCost1),
   Cost1 is ArgsCost1 + SymbolCost1,
   weight(Symbol2,NestingLevel,SymbolCost2),
   Cost2 is ArgsCost2 + SymbolCost2,
   LGG =.. [Symbol|ArgsLGG].
    
%------------------------------------------------------------------------------
%----Do equality with symmetry
lgg_atom('$tptp_equal'(LHS1,RHS1),'$tptp_equal'(LHS2,RHS2),LGG,AtomCost1,
AtomCost2,Cost):-
    !,
    lgg_list([LHS1,RHS1],[LHS2,RHS2],1,_,1,ArgsLGG1,ArgsCost11,ArgsCost21),
    Cost1 is sqrt((ArgsCost11*ArgsCost11) + (ArgsCost21*ArgsCost21)),
    lgg_list([LHS1,RHS1],[RHS2,LHS2],1,_,1,ArgsLGG2,ArgsCost12,ArgsCost22),
    Cost2 is sqrt((ArgsCost12*ArgsCost12) + (ArgsCost22*ArgsCost22)),
    (   Cost1 < Cost2
    ->  (   LGG =.. ['$tptp_equal'|ArgsLGG1],
            AtomCost1 = ArgsCost11,
            AtomCost2 = ArgsCost21,
            Cost = Cost1 )
    ;   (   LGG =.. ['$tptp_equal'|ArgsLGG2],
            AtomCost1 = ArgsCost12,
            AtomCost2 = ArgsCost22,
            Cost = Cost2 ) ).

%----If they have an LGG, Pythagoras (go around the mountain)
lgg_atom(Atom1,Atom2,LGG,AtomCost1,AtomCost2,Cost):-
    lgg_term(Atom1,Atom2,1,_,0,LGG,AtomCost1,AtomCost2),
    !,
    Cost is sqrt((AtomCost1*AtomCost1) + (AtomCost2*AtomCost2)).

%----Otherwise sum of weights (climb the mountain)
lgg_atom(Atom1,Atom2,none,AtomCost1,AtomCost2,Cost):-
   weight(Atom1,0,AtomCost1),
   weight(Atom2,0,AtomCost2),
   Cost is AtomCost1 + AtomCost2.

%------------------------------------------------------------------------------
lgg_atoms_distances(Atoms1,Atoms2,AtomDistances):-
    findall(Atom1-Atom2-->Cost,
        (   member(Atom1,Atoms1),
            member(Atom2,Atoms2),
            copy_term(Atom1,Copy1),
            copy_term(Atom2,Copy2),
            lgg_atom(Copy1,Copy2,_,_,_,Cost)),
        AtomDistances).
%------------------------------------------------------------------------------
hausdorff(AtomDistances,HausdorfDistance):-
    numbervars(AtomDistances,0,_),

    tptp2X_findall_setof1(Atom1,member(Atom1-_-->_,AtomDistances),Atoms1),
    findall(Atom1-Atom1Distances,
        (   member(Atom1,Atoms1),
            findall(Distance1,
                member(Atom1-_-->Distance1,AtomDistances),
                Atom1Distances ) ),
            Atoms1Distances ),
    findall(MinDistance1,
        (   member(Atom1-Distances1,Atoms1Distances),
            min_list(Distances1,MinDistance1) ),
        Atoms1MinDistances ),
    max_list(Atoms1MinDistances,MaxMinDistance1),

    tptp2X_findall_setof1(Atom2,member(_-Atom2-->_,AtomDistances),Atoms2),
    findall(Atom2-Atom2Distances,
        (   member(Atom2,Atoms2),
            findall(Distance2,
                member(_-Atom2-->Distance2,AtomDistances),
                Atom2Distances ) ),
            Atoms2Distances ),
    findall(MinDistance2,
        (   member(Atom2-Distances2,Atoms2Distances),
            min_list(Distances2,MinDistance2) ),
        Atoms2MinDistances ),
    max_list(Atoms2MinDistances,MaxMinDistance2),

    max_list([MaxMinDistance1,MaxMinDistance2],HausdorfDistance).
%------------------------------------------------------------------------------
%----Do all pairs of formulae
lgg_annotated_formulae_distances([_],[]):-
    !.

lgg_annotated_formulae_distances([AnnotatedFormula1,AnnotatedFormula2|
RestOfFormulae],[Name1-Name2-->Distance|RestOfDistances]):-
    AnnotatedFormula1 =.. [_,Name1|_],
    AnnotatedFormula2 =.. [_,Name2|_],
    extract_atoms_from_formulae([AnnotatedFormula1],_,_,no,Atoms1),
    extract_atoms_from_formulae([AnnotatedFormula2],_,_,no,Atoms2),
    lgg_atoms_distances(Atoms1,Atoms2,AtomDistances),
%DEBUG write('AtomDistances '),write(AtomDistances),nl,
    hausdorff(AtomDistances,Distance),
    lgg_annotated_formulae_distances([AnnotatedFormula2|RestOfFormulae],
RestOfDistances).

%------------------------------------------------------------------------------
write_distances([]).

write_distances([Name1-Name2-->Distance|Rest]):-
    pad_atom(Name1,right,20,' ',PaddedName1),
    pad_atom(Name2,right,20,' ',PaddedName2),
    pad_number(Distance,4,FormattedDistance),
    pad_atom(FormattedDistance,left,8,' ',PaddedDistance),
    write(PaddedName1),write(' '),
    write(PaddedName2),write(' '),
    write(PaddedDistance),nl,
    write_distances(Rest).
%------------------------------------------------------------------------------
pad_number(DecimalNumber,DecimalPlaces,FormattedNumber):-
    atom_chars(DecimalNumber,NumberChars),
    append(IntegerPartChars,['.'|FractionalPartChars],NumberChars),
    !,
    atom_chars(FractionalPart,FractionalPartChars),
    pad_number_fractional_part(FractionalPart,DecimalPlaces,
PaddedFractionalPart),
    atom_chars(IntegerPart,IntegerPartChars),
    atomic_list_concat([IntegerPart,'.',PaddedFractionalPart],
FormattedNumber).

pad_number(IntegerNumber,DecimalPlaces,FormattedNumber):-
    pad_number_fractional_part('',DecimalPlaces,Zeros),
    atomic_list_concat([IntegerNumber,'.',Zeros],FormattedNumber).

%------------------------------------------------------------------------------
pad_number_fractional_part(FractionalPart,DecimalPlaces,PaddedFractionalPart):-
    atom_length(FractionalPart,FractionalPartLength),
    FractionalPartLength =< DecimalPlaces,
    !,
    ZerosNeeded is DecimalPlaces - FractionalPartLength,
    pad_atom(FractionalPart,right,ZerosNeeded,'0',PaddedFractionalPart).

pad_number_fractional_part(FractionalPart,DecimalPlaces,PaddedFractionalPart):-
    length(LengthList,DecimalPlaces),
    atom_chars(FractionalPart,FractionalPartChars),
    append(LengthList,_,FractionalPartChars),
    atom_chars(PaddedFractionalPart,LengthList).

%------------------------------------------------------------------------------
pad_atom(Atom,_,Size,_,Atom):-
    atom_length(Atom,AtomLength),
    AtomLength >= Size,
    !.

pad_atom(Atom,Side,Size,Character,PaddedAtom):-
    atom_length(Atom,AtomLength),
    SpacesNeeded is Size - AtomLength,
    space_atom(SpacesNeeded,Character,[],SpacesAtom),
    (   Side == right
    ->  atom_concat(Atom,SpacesAtom,PaddedAtom)
    ;   atom_concat(SpacesAtom,Atom,PaddedAtom) ).

%------------------------------------------------------------------------------
space_atom(SpacesNeeded,_,ListOfSpaces,SpacesAtom):-
    SpacesNeeded =< 0,
    !,
    atom_chars(SpacesAtom,ListOfSpaces).

space_atom(SpacesNeeded,Character,ListSoFar,SpacesAtom):-
    LessSpacesNeeded is SpacesNeeded - 1,
    space_atom(LessSpacesNeeded,Character,[Character|ListSoFar],SpacesAtom).

%------------------------------------------------------------------------------
lgg_file_distances(TPTPFileName,Distances):-
    declare_TPTP_operators,
    read_formulae_from_file(TPTPFileName,AnnotatedFormulae,_),
    lgg_annotated_formulae_distances(AnnotatedFormulae,LocalDistances),
    ( Distances == print -> (   
        write_distances(LocalDistances),
        nl 
    ) ; (   
        Distances = LocalDistances )
    ).
%------------------------------------------------------------------------------
