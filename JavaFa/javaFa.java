package JavaFa;

import java.util.*;

/*
Clean up b4 we create github
 */

    //Interface class for any fsa , both nfa and dfa,
    abstract class Fa {
        Map<Integer, State> states;
        HashSet<Character> alphabet;
        ArrayList<Transition> transition;  //change to transitions
        State startState;
        Map<Integer, Map<Character, HashSet<Integer>>> transitionTable; //eg,  q1: 'a':[q2,q3], this isnt directly what happens for a dfa b
        //ut in theory it can work as it would just be q1: 'a': [q2] - just pointing to one value in the set

        public Fa() {
            states = new HashMap<>();
            alphabet = new HashSet<>();
            transition = new ArrayList<>();
            transitionTable = new HashMap<>();
        }

        public void addState(State state) {
            if (state.checkStart()) {
                startState = state; 
            }
            
            states.put(state.getId(), state);
            if (!transitionTable.containsKey(state.getId()))
                transitionTable.put(state.getId(), new HashMap<>());
        }

        abstract void addTransition(int to, int from, char arg);  //assuming these will be implemented differently depending on the fa
        abstract Boolean accepts(String input); 
    }

    class State {
        private Integer id;
        private Boolean isStart;
        private Boolean isFinal;

        public State(int id, boolean isStart, boolean isFinal) {
            this.id = id;
            this.isStart = isStart;
            this.isFinal = isFinal;
        }

        public Integer getId() { 
            return id; 
        }
        
        public Boolean checkStart() {
            return isStart;
        }
        
        public Boolean checkFinal() {
            return isFinal;
        }
    }

    class Transition {
        private State to;
        private State from;
        private Character arg;

        public Transition(State to, State from, Character arg) {
            this.to = to;
            this.from = from;
            this.arg = arg;
        }

        public State getTo() { return to; }
        public State getFrom() { return from; }
        public Character getArg() { return arg; }
    }

    class Nfa extends Fa { //Nfa class that implements these features
        
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
                if (visited.contains(current)) 
                    continue;
                    
                visited.add(current);
                
                // c epsilon transitions  = (\0)
                if (transitionTable.get(current) != null) {
                    if (transitionTable.get(current).containsKey('\0')) { 
                        for (Integer next : transitionTable.get(current).get('\0')) { // adding all possible states that can be travelled to with ep closer
                            if (!visited.contains(next)) {
                                stack.push(next);
                            }
                        }
                    }
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

        Boolean isDeterministic() { // looking for if it sees epsilon closure or multiple transitions from one state
            
            for (Integer stateId : states.keySet()) {
                Map<Character, HashSet<Integer>> trans = transitionTable.get(stateId);
                if (trans != null && trans.containsKey('\0')) {
                    return false;  
                }
            }
                   for (Integer stateId : states.keySet()) {
                Map<Character, HashSet<Integer>> trans = transitionTable.get(stateId);
                if (trans != null) {
                    for (Character symbol : trans.keySet()) {
                        if (trans.get(symbol).size() > 1) {
                            return false;  // multiple transitions
                        }
                    }
                }
            }
            
            return true;
        }

        Nfa convertToDFA() {
            Nfa dfa = new Nfa();
            Map<HashSet<Integer>, Integer> stateMap = new HashMap<>(); // like transition table but specifcally for dfa, transition table in interface can still also work for dfa
            ArrayList<HashSet<Integer>> worklist = new ArrayList<>(); // what states still need to be worked on
            int count = 0; // new ids
            
            HashSet<Integer> startSet = epsilonClosure(startState.getId());
            worklist.add(startSet);
            stateMap.put(startSet, count);
            
       
          
             boolean startAccepting = false;
            for (Integer id : startSet) {
                if (states.get(id).checkFinal()) {
                    startAccepting = true;
                    break;
                }
            }
            
            dfa.addState(new State(count, true, startAccepting));
            count++;
            
            int i = 0;
            while (i < worklist.size()) {
                HashSet<Integer> currentSet = worklist.get(i);
                i++;
                
                
                for (Character c : alphabet) {
                    if (c == '\0') continue;  //skips all eps
                    
                    HashSet<Integer> nextSet = new HashSet<>();
                    
                    // 
                    for (Integer stateId : currentSet) {
                        HashSet<Integer> reachable = getNext(stateId, c);
                        nextSet.addAll(reachable);
                    }
                    
                    if (nextSet.isEmpty()) 
                        continue;
                    
                    
                    HashSet<Integer> closedSet = new HashSet<>();
                    for (Integer stateId : nextSet) { // all states possible to reach from states which are next
                        HashSet<Integer> closure = epsilonClosure(stateId);
                        closedSet.addAll(closure);
                    }
                    
                    if (!stateMap.containsKey(closedSet)) {
                        stateMap.put(closedSet, count);
                        worklist.add(closedSet);
                        boolean accepting = false;
                        for (Integer id : closedSet) {
                            if (states.get(id).checkFinal()) {
                                accepting = true;
                                break;
                            }
                        }
                        
                        dfa.addState(new State(count, false, accepting));
                        count++;
                    }
                    
 
                    int fromState = stateMap.get(currentSet);
                    int toState = stateMap.get(closedSet);
                    dfa.addTransition(toState, fromState, c);
                }
            }
            
            return dfa;
        }

        @Override
        Boolean accepts(String input) { //added accepts last as other funcitons example epsilon closure and next make the implementation far easier
   
            HashSet<Integer> currentStates = epsilonClosure(startState.getId());
            

            for (int i = 0; i < input.length(); i++) {
                char c = input.charAt(i);
                HashSet<Integer> nextStates = new HashSet<>();
                
                //get all states
                for (Integer stateId : currentStates) {
                    HashSet<Integer> reachable = getNext(stateId, c);
                    nextStates.addAll(reachable);
                }
                

                if (nextStates.isEmpty()) {
                    return false;
                }
                
                //  epsiln closure
                HashSet<Integer> newCurrentStates = new HashSet<>();
                for (Integer stateId : nextStates) {
                    HashSet<Integer> closure = epsilonClosure(stateId);
                    newCurrentStates.addAll(closure);
                }
                
                currentStates = newCurrentStates;
            }
            
            // check if we ended in an accepting state
            for (Integer stateId : currentStates) {
                if (states.get(stateId).checkFinal()) {
                    return true;
                }
            }
            
            return false;
        }
    }

public class javaFa {
    public static void main(String[] args) {

        Nfa nfa = new Nfa();
        
        nfa.addState(new State(0, true, false));   // start
        nfa.addState(new State(1, false, true));   // accept

        nfa.addTransition(1, 0, 'a');
        
        // Test 1
        System.out.println("'a': " + nfa.accepts("a"));   // true
        System.out.println("'b': " + nfa.accepts("b"));   // false
        
        
        // Test 2
        Nfa nfa2 = new Nfa();
        nfa2.addState(new State(0, true, false));
        nfa2.addState(new State(1, false, true));
        nfa2.addTransition(0, 0, 'x');  
        nfa2.addTransition(1, 0, 'x'); 
        
        System.out.println("\nBefore: " + nfa2.isDeterministic());  // false
        Nfa dfa = nfa2.convertToDFA();
        System.out.println("After: " + dfa.isDeterministic());       // true
        System.out.println("'x': " + dfa.accepts("x"));              // true
    }
}