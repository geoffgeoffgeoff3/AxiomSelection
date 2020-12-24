%------------------------------------------------------------------------------
%----Remove all elements upto the last delimiter
tptp2X_list_tail([],_,[],not_found):-
    !.

%----Check if the delimiter is further down the list
tptp2X_list_tail([_|ListTail],Delimiter,TailTail,found):-
    tptp2X_list_tail(ListTail,Delimiter,TailTail,found),
    !.

%----Check if this is the delimiter
tptp2X_list_tail([Delimiter|ListTail],Delimiter,ListTail,found):-
    !.

%----Otherwise pass back this list
tptp2X_list_tail(List,_,List,not_found).
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
tptp2X_append(Head,Tail,HeadTail):-
    append(Head,Tail,HeadTail).
%------------------------------------------------------------------------------
%----Simple select. Renamed to avoid confusion with builtins.
tptp2X_select(Element,List,Tail):-
    select(Element,List,Tail).
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
tptp2X_member(Element,List):-
    member(Element,List).
%------------------------------------------------------------------------------
%----Exact member without unification. Only a test, not for extraction
tptp2X_exact_member(Element,[Head|_]):-
    Element == Head.

tptp2X_exact_member(Element,[_|Tail]):-
    tptp2X_exact_member(Element,Tail).
%------------------------------------------------------------------------------
%----Simple length. Renamed to avoid confusion with builtins.
tptp2X_length(List,Length):-
    length(List,Length).
%------------------------------------------------------------------------------
tptp2X_atom_length(Atom,AtomLength):-
     atom_chars(Atom,AtomList),
     length(AtomList,AtomLength).
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
one_member(Element,List):-
    member(Element,List),
    !.
%------------------------------------------------------------------------------
%----Extracts the .XXX from a filename
tptp2X_extension(FullName,Extension):-
    atom_chars(FullName,FullChars),
    tptp2X_append(_,['.'|ExtensionChars],FullChars),
%----Force the last . to be taken
    \+ tptp2X_member('.',ExtensionChars),
    !,
    atom_chars(Extension,['.'|ExtensionChars]).

%----No extension
tptp2X_extension(_,'').
%------------------------------------------------------------------------------
%----Procedure to do the same as the UNIX basename command
tptp2X_basename(FullName,Suffix,Basename):-
    atom_chars(FullName,FullNameChars),
    atom_chars(Suffix,SuffixChars),
%----Delete upto last /
    tptp2X_list_tail(FullNameChars,'/',FullNameTailChars,_),
%----Try to delete suffix from list
    (tptp2X_append(BasenameChars,SuffixChars,FullNameTailChars) ->
        atom_chars(Basename,BasenameChars);
%----If not possible, then ignore the suffix
        atom_chars(Basename,FullNameTailChars)).
%------------------------------------------------------------------------------
%----Procedure to do the same as the UNIX dirname command
tptp2X_dirname(FullName,Dirname):-
    atom_chars(FullName,FullNameChars),
%----Remove the tptp2X_basename
    tptp2X_basename(FullName,'',Basename),
    atom_chars(Basename,BasenameChars),
%----Find what is before the tptp2X_basename, if anything
    (tptp2X_append(DirnameChars,['/'|BasenameChars],FullNameChars) ->
        atom_chars(Dirname,DirnameChars);
%----If no directory then it's the current one
        Dirname = '.').
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
    pad_atom(FractionalPart,right,DecimalPlaces,'0',PaddedFractionalPart).

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
    CharactersNeeded is Size - AtomLength,
    space_atom(CharactersNeeded,Character,[],SpacesAtom),
    ( Side == right
    ->  atom_concat(Atom,SpacesAtom,PaddedAtom)
    ;   atom_concat(SpacesAtom,Atom,PaddedAtom) 
    ).

%------------------------------------------------------------------------------
space_atom(SpacesNeeded,_,ListOfSpaces,SpacesAtom):-
    SpacesNeeded =< 0,
    !,
    atom_chars(SpacesAtom,ListOfSpaces).

space_atom(SpacesNeeded,Character,ListSoFar,SpacesAtom):-
    LessSpacesNeeded is SpacesNeeded - 1,
    space_atom(LessSpacesNeeded,Character,[Character|ListSoFar],SpacesAtom).

%------------------------------------------------------------------------------

