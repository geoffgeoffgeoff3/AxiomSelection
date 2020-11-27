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

