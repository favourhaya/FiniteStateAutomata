:- dynamic state/1.
:- dynamic startState/1.
:- dynamic finalState/1.
:- dynamic transition/3.


addState(State, IsStart, IsFinal) :-
    assertz(state(State)),
    (IsStart = true -> assertz(startState(State)); true),
    (IsFinal = true -> assertz(finalState(State)); true).

addTransition(From, Symbol, To) :-
    assertz(transition(From, Symbol, To)).

closure(Start, Answer) :-
    getClosure([Start], [Start], Answer).

getClosure([], Visited, Visited).

getClosure([Curr|Rest], Visited, Answer) :-
    findall(Next,
        (transition(Curr, ep, Next),
         \+ member(Next, Visited)),
        AllNext),
    append(AllNext, Rest, NextStates),
    append(AllNext, Visited, NewVisited),
    getClosure(NextStates, NewVisited, Answer).

next(Start, Symbol, Answer) :-
    Symbol \= ep,
    closure(Start, CurrentStates),
    
    findall(Next,
        (member(S, CurrentStates),
         transition(S, Symbol, Next)),
        AllNext),

    findall(Possible,
        (member(S, AllNext),
         closure(S, NewStates),
         member(Possible, NewStates)),
        AllPossible),
    list_to_set(AllPossible, Answer).

deterministic :-
    \+ transition(_, ep, _),
    \+ (state(From),
        transition(From, Sym, To),
        transition(From, Sym, To2),
        To \= To2).

toDFA(dfa(AllStates, DFAStart, AcceptStates, AllTransitions)) :-

    findall(S, startState(S), NFAStarts),
    epsilon_closure_set(NFAStarts, DFAStart),
    
    findall(Sym, (transition(_, Sym, _), Sym \= ep), AllSymbols),
    list_to_set(AllSymbols, Alphabet),
    
 
    Queue = [DFAStart],
    Visited = [DFAStart],

    process(Queue, Visited, Alphabet, AllStates, AllTransitions),
    

    findall(DFAState,
            (member(DFAState, AllStates),
             member(NFAState, DFAState),
             finalState(NFAState)),
            AcceptStates).


process([], Visited, _, Visited, []).


process([Current|Rest], Visited, Alphabet, AllStates, AllTransitions) :-
    %lkjn
    true. 


epsilon_closure_set(States, ClosureSet) :-
    findall(S,
            (member(State, States),
             closure(State, Closure),
             member(S, Closure)),
            AllStates),
    list_to_set(AllStates, ClosureSet).



