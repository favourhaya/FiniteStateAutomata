
:- consult(fa).


:- example.

runTests :-
    % Test 1: closure
    closure(0, C),
    format('All closures from start: ~w~n', [C]),
    
    % Test 2: next
    next(4, b, N),
    format('All next states from 4 using b: ~w~n', [N]),
    
    % Test 3: deterministic
    format('Current FSA is deterministic: '),
    (deterministic -> writeln('true') ; writeln('false')),
    
    % Test 4: toDFA and check deterministic
    toDFA(DFA),
    format('FSA after toDFA is deterministic: '),
    (deterministic(DFA) -> writeln('true') ; writeln('false')).


:- runTests.
:- halt.

