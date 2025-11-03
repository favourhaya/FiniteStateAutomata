package javaVersion;
import java.util.*;

public class javaFa {
    abstract class Fa{
        Map<Integer, State> states;
        HashSet<Character> alphabet;
        ArrayList<Transition> transition;
        State startState;
        Map<Integer, Map<Character,HashSet<Integer>>> transitionTable;

        public Fa(){
            states = new HashMap<>();
            alphabet = new HashSet<>();
            transition =  new ArrayList<>();
            transitionTable = new HashMap<>();
        }

        public void addState(State state){
            if (state.checkStart()){
                startState = state;
            }

            states.put(state.getId(), state);
            if (!transitionTable.containsKey(state.getId())){
            transitionTable.put(state.getId(), new HashMap<>());}
        }

        abstract void addTransition(int to , int from , char arg);

        abstract Boolean accepts(String input);
    }

    class State{
        private Integer id;
        private Boolean isStart;
        private Boolean isFinal;

        public State(int id, boolean isStart, boolean isFinal){
            this.id = id;
            this.isStart = isStart;
            this.isFinal = isFinal;
        }

        public Integer getId(){ return id; } 
        public Boolean checkStart(){return isStart;}
        public Boolean checkFinal(){
            return isFinal;}
    }

    class Transition{
        private State to;
        private State from;
        private Character arg;

          public Transition(State to, State from, Character arg){
            this.to = to;
            this.from = from;
            this.arg = arg;
        }

        public State getTo(){ return to;}
        public State getFrom(){return from;}
        public Character getArg(){return arg;}
    }

   class Nfa extends Fa {
    
    @Override
void addTransition(int to, int from, char arg) {
    State toState = states.get(to);
    State fromState = states.get(from);

    alphabet.add(arg);
    transition.add(new Transition(toState, fromState, arg));
    
    if (!transitionTable.containsKey(from)) {
    transitionTable.put(from, new HashMap<>());
    }
    if (!transitionTable.get(from).containsKey(arg)) {
    transitionTable.get(from).put(arg, new HashSet<>());
    }
     transitionTable.get(from).get(arg).add(to);
}

    private HashSet<Integer> epsilonClosure(Integer stateId) {
        HashSet<Integer> visited = new HashSet<>();
        Stack<Integer> stack = new Stack<>();
        stack.push(stateId);

        while (!stack.isEmpty()) {
            Integer current = stack.pop();
            if (visited.contains(current)) continue;
            visited.add(current);

            // check epsilon transitions
            if (transitionTable.get(current) != null) {
                if (transitionTable.get(current).containsKey(null)) {
                for (Integer next : transitionTable.get(current).get(null)) {
                if (!visited.contains(next)) {
                    stack.push(next); 
                    
                }
            
                }}
            }
            }
        return visited;
    }

    private HashSet<Integer> getNext(Integer stateId, char symbol) {
        HashSet<Integer> result = new HashSet<>();
        if (transitionTable.get(stateId) != null) {
            if (transitionTable.get(stateId).containsKey(symbol)) {
            result.addAll(transitionTable.get(stateId).get(symbol));
            }
        }
        return result;
    }

    Boolean isDeterministic() {
        // check epsilon transitions first
        for (Integer stateId : states.keySet()) {
            Map<Character, HashSet<Integer>> trans = transitionTable.get(stateId);
            if (trans != null && trans.containsKey(null)) {
            return false;
            }
        }
        
        // check multiple transitions on same symbol
        for (Integer stateId : states.keySet()) {
            Map<Character, HashSet<Integer>> trans = transitionTable.get(stateId);
            if (trans != null) {
                for (Character symbol : trans.keySet()) {
                    if (trans.get(symbol).size() > 1) {
                    return false;
                    }
                } }}
        return true;
    }

    Nfa convertToDFA() {
        Nfa dfa = new Nfa();
        Map<HashSet<Integer>, Integer> stateMap = new HashMap<>();
        ArrayList<HashSet<Integer>> worklist = new ArrayList<>();
        int counter = 0;


        HashSet<Integer> startSet = epsilonClosure(startState.getId());
        worklist.add(startSet);
        stateMap.put(startSet, counter);


        boolean startAccepting = false;
        for (Integer id : startSet) {
            if (states.get(id).checkFinal()) {
            startAccepting = true;
            //return false;
                break;
            }
        }
        dfa.addState(new State(counter, true, startAccepting));
        counter++;

        int index = 0;
        while (index < worklist.size()) {
            HashSet<Integer> currentSet = worklist.get(index);
            index++;

            for (Character c : alphabet) {
                HashSet<Integer> nextSet = new HashSet<>();

                // find all reachable states
                for (Integer stateId : currentSet) {
                 HashSet<Integer> reachable = getNext(stateId, c);
                 nextSet.addAll(reachable);
                }

                if (nextSet.isEmpty()) continue;

                // apply epsilon closure
                HashSet<Integer> closedSet = new HashSet<>();
                for (Integer stateId : nextSet) {
                 HashSet<Integer> closure = epsilonClosure(stateId);
                 closedSet.addAll(closure);
                }

                // check if we've seen this set before
                if (!stateMap.containsKey(closedSet)) {
                    stateMap.put(closedSet, counter);
                    worklist.add(closedSet);
                    boolean accepting = false;
                    for (Integer id : closedSet) {
                        if (states.get(id).checkFinal()) {
                            accepting = true;
                            break;
                        }}

                    dfa.addState(new State(counter, false, accepting));
                    counter++;
                }

                // add transition
                int fromState = stateMap.get(currentSet);
                int toState = stateMap.get(closedSet);
                dfa.addTransition(toState, fromState, c);
            }
        }

        return dfa;
    }

    @Override
    Boolean accepts(String input) {
        HashSet<Integer> currentStates = epsilonClosure(startState.getId());

        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            HashSet<Integer> nextStates = new HashSet<>();

            for (Integer stateId : currentStates) {
             HashSet<Integer> reachable = getNext(stateId, c);
            nextStates.addAll(reachable);
            }

            if (nextStates.isEmpty()) {
             return false;
            }

            // apply epsilon closure to all next states
            HashSet<Integer> newCurrentStates = new HashSet<>();
            for (Integer stateId : nextStates) {
             HashSet<Integer> closure = epsilonClosure(stateId);
                newCurrentStates.addAll(closure);
            }
            
            currentStates = newCurrentStates;
        }

        // check if any current state is final
        for (Integer stateId : currentStates) {
            if (states.get(stateId).checkFinal()) {
                return true;
            }}

        return false;
    }
}
}