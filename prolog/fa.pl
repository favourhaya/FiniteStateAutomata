:- dynamic state/1, startState/1, finalState/1, transition/3.

addState(State, IsStart, IsFinal) :-
    assertz(state(State)), 
    (IsStart = true -> assertz(startState(State)); true), 
    (IsFinal = true -> assertz(finalState(State)); true).

addTransition(From, Symbol, To) :-
   ( state(From) -> true ; assertz(state(From))),
    (state(To) -> true ; assertz(state(To))),
    assertz(transition(From, Symbol, To)).



closure(S, Result) :-
    closureTraversal([S], [S], Result).


% epsilon closure computation cause users should only put in the start in the actual closure function

closureTraversal([], V, V).



closureTraversal([H|T], Visited, Result) :-
   % findall(N,     %setof puts it in order but it could mess up logic
    %    (transition(H, eps, N), \+ member(N, Visited)), 
    %    NextList),

    findall(N,     %setof puts it in order but it could mess up logic
        (transition(H, eps, N), \+ member(N, Visited)), 
        NextList),
    append(NextList, T, RestStates),
    append(NextList, Visited, V2),
    closureTraversal(RestStates, V2, Result).



next(Start, Sym, Result) :-
    Sym \= eps,
    closure(Start, CurrStates),
    findall(N,  (member(St, CurrStates), transition(St, Sym, N)),
        NextStates),
    
    % get closure of all next states
    setof(P, (member(X, NextStates), closure(X, Cls),  member(P, Cls)),
        Result).


accepts(String) :-
    findall(S, startState(S), Starts),
    Starts \= [],
    closureSet(Starts, Init),
    stateQueue(Init, String).

% base case - empty string
stateQueue(States, []) :- member(St, States), finalState(St).

stateQueue(States, [Sym|Rest]) :-
    setof(NS,
    (member(CS, States),    next(CS, Sym, NSs),    member(NS, NSs)),
        NextSet),
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
    \+ member(transition(_, eps, _), Trans),
    \+ (member(transition(F, Sym, T1), Trans),
        member(transition(F, Sym, T2), Trans),
        T1 \= T2).

process([], V, _, V, []).

process([H|Rest],V,A,States,Trans):-
    findall(transition(H,Sym,To),
    (member(Sym,A),
    findall(R, (member(NFA, H), next(NFA, Sym, Rs),
     member(R, Rs)),
    List),
     list_to_set(List, To),
    To \= []),
        T),

    findall(To, member(transition(_,_,To),T), New),
    findall(NS, (member(NS,New), \+ member(NS,V)),X),

    append(Rest,X,Queue),
    append(V,X,Visit),
    process(Queue, Visit, A, States, RestTrans),
    append(T, RestTrans,Trans).

toDFA(dfa(Sts, DFAStart, Fins, Trans)) :-
    startState(S),
    closure(S,DFAStart),

    setof(Sym,(transition(_,Sym,_),Sym \= eps),Alphabet),
    
    Q = [DFAStart],
    V = [DFAStart],

    process(Q, V, Alphabet,Sts,Trans),
    findall(DFASt,  
        (member(DFASt, Sts),  
         member(NFASt, DFASt), 
         finalState(NFASt)),
        Fins).


closureSet(Sts, All) :- 
    setof(S,
        (member(St, Sts), closure(St, Cls), member(S, Cls)),
        All).


example :-
    addState(0,true,false),
    addState(1,false,false),
    addState(2,false,false),
    addState(3,false,false),
    addState(4,false,false),
    addState(5,false,false),
    addState(6,false,false),
    addState(7,false,false),
    addState(8,false,false),
    addState(9,false,false),
    addState(10,false,true),



    addTransition(0,eps,1),
    addTransition(0,eps,7),
    addTransition(1,eps,2),
    addTransition(1,eps,4),
    addTransition(2,a,3),
    addTransition(4,b,5),
    addTransition(3,eps,6),
    addTransition(5,eps,6),
    addTransition(6,eps,7),
    addTransition(7,a,8),
    addTransition(8,b,9),
    addTransition(9,b,10).
    
