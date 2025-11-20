:- dynamic state/1, startState/1, finalState/1, transition/3.

addState(State, IsStart, IsFinal) :-
    assertz(state(State)), 
    (IsStart = true -> assertz(startState(State)); true), 
    (IsFinal = true -> assertz(finalState(State)); true).

addTransition(From, Symbol, To) :-
    state(From) -> true ; assertz(state(From)),
    state(To) -> true ; assertz(state(To)),
    assertz(transition(From, Symbol, To)).

% epsilon closure computation cause users should only put in the start in the actual closure function
closureTraversal([], V, V).
closureTraversal([H|T], Visited, Result) :-
    findall(N,
        (transition(H, eps, N), \+ member(N, Visited)), 
        NextList),
    append(NextList, T, RestStates),
    append(NextList, Visited, V2),
    closureTraversal(RestStates, V2, Result).

closure(S, Result) :-
    closureTraversal([S], [S], Result).


next(Start, Sym, Result) :-
    Sym \= eps,
    closure(Start, CurrStates),
    findall(N,  
        (member(St, CurrStates), transition(St, Sym, N)),
        NextStates),
    
    % get closure of all next states
    findall(P,
        (member(X, NextStates), 
         closure(X, Cls), 
         member(P, Cls)),
        AllPoss),
    list_to_set(AllPoss, Result).


accepts(String) :-
    findall(S, startState(S), Starts),
    Starts \= [],
    closureSet(Starts, Init),
    stateQueue(Init, String).

% base case - empty string
stateQueue(States, []) :- member(St, States), finalState(St).

stateQueue(States, [Sym|Rest]) :-
    findall(NS,
        (member(CS, States),   
         next(CS, Sym, NSs),   
         member(NS, NSs)),
        AllNext),
    list_to_set(AllNext, NextSet),
    NextSet \= [],
    stateQueue(NextSet, Rest).


% both check to main rules, theres no point in checking if theres multiple start staes anyway
deterministic :- 
    \+ transition(_, eps, _),
    \+ (state(S), 
        transition(S, Sym, T1), 
        transition(S, Sym, T2),
        T1 \= T2).

deterministic(dfa(States, Start, _, Trans)) :-
    \+ member(trans(_, eps, _), Trans),
    \+ (member(trans(F, Sym, T1), Trans),
        member(trans(F, Sym, T2), Trans),
        T1 \= T2).



computeNextDFAState(CurrDFA, Sym, NextDFA) :-
    findall(R,
        (member(NFA, CurrDFA), 
         next(NFA, Sym, Rs), 
         member(R, Rs)),
        All),
    list_to_set(All, NextDFA).



toDFA(dfa(AllSts, DFAStart, AcceptSts, AllTrans)) :-
    findall(S, startState(S), NFAStarts),
    closureSet(NFAStarts, DFAStart),
    
    findall(Sym, (transition(_, Sym, _), Sym \= eps), Syms),
    list_to_set(Syms, Alphabet),

    Q = [DFAStart],
    V = [DFAStart],
    processQueue(Q, V, Alphabet, AllSts, AllTrans),
    
    findall(DFASt,  
        (member(DFASt, AllSts),  
         member(NFASt, DFASt), 
         finalState(NFASt)),
        AcceptSts).


%have to explain each find all as its kind of confusing
processQueue([], V, _, V, []).
processQueue([Curr|Rest], V, Alpha, AllSts, AllTrans) :-
    findall(trans(Curr, Sym, NextDFA),
        (member(Sym, Alpha),
         computeNextDFAState(Curr, Sym, NextDFA),
         NextDFA \= []),
        Trans),
    
    findall(NS, member(trans(_, _, NS), Trans), NewSts),
    findall(NS, 
        (member(NS, NewSts), \+ member(NS, V)),
        ToVisit),
    
    append(Rest, ToVisit, Q2),
    append(V, ToVisit, V2),
    processQueue(Q2, V2, Alpha, AllSts, RestTrans),
    append(Trans, RestTrans, AllTrans).


closureSet(Sts, ResultSet) :- 
    findall(S,
        (member(St, Sts), 
         closure(St, Cls), 
         member(S, Cls)),
        All),
    list_to_set(All, ResultSet).