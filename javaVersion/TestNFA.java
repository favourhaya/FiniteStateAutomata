package javaVersion;

public class TestNFA {
    public static void main(String[] args) {
        javaFa fa = new javaFa();
        javaFa.Nfa nfa = fa.new Nfa();
        
        javaFa.State s0 = fa.new State(0, true, false);
       
         javaFa.State s1 = fa.new State(1, false, true);
        
        nfa.addState(s0);

             nfa.addState(s1);
             nfa.addTransition(1, 0, 'a');
        
             System.out.println(nfa.accepts("a"));

             System.out.println(nfa.accepts("b"));
        
        javaFa.Nfa nfa2 = fa.new Nfa();
        javaFa.State s2 = fa.new State(0, true, false);

        javaFa.State s3 = fa.new State(1, false, false);
        
        javaFa.State s4 = fa.new State(2, false, true);
        
        nfa2.addState(s2);
        
         nfa2.addState(s3);

        nfa2.addState(s4);
        
        nfa2.addTransition(1, 0, 'a');
      nfa2.addTransition(2, 1, 'b');
        
        System.out.println(nfa2.accepts("ab"));

        System.out.println(nfa2.accepts("a"));
    }
}