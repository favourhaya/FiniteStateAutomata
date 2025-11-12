dynamic state/1.
dynamic startState/1.
dynamic finalState/1.
dynamic transition/3.


addState(State, IsStart,IsFinal) :-
    assertz(state(State)),
    (IsStart = true -> assertz(startState(State)); true),
    (IsFinal = true -> assertz(finalState(State)); true).

addTransition(From,Symbol,To) :-
    assertz(transition(From,Symbol,To)).

closure(Start,Answer) :-
    getClosure([Start],[Start],Answer).

getClosure([],visited,Visited).


getClosure([Curr|Rest],Visited,Answer) :-
    findall(Next,
    (transition(curr,ep,Next),
    \+ member (Next,Visited)),
    allNext),
    append(Rest,allNext,NextStates),
    append(Rest,Visited,NewVisited),
    getClosure(NextStates,NewVisited,Answer).

next(Start,Symbol,Answer) :-
    Symbol \= ep,
    closure(start,currentStates),
    
    findall(Next,
    (member(S,currentStates),
    transition(S, symbol,Next)),
    allNext),

    finall(Possible,
    (member(S,allNext),
    closure(S,NewStates),
    member(Possible,NewStates)
    ),
    allPossible),
    list_to_set(allPossible,Answer).

deterministic :-
    transition(_ , ep , _),
    
    +\(state(From),
        transition(From,Sym, To),
        transition(From,Sym, To2),
        To \= To2).

