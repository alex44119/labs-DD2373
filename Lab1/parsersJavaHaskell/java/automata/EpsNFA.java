package automata;

import java.util.*;

public class EpsNFA extends Automaton<Integer, Character> {
    
    public final static Character EPSILON = '\u03B5';
    
    
    public EpsNFA() {
        initial = 0;
    }
    
    
    public Integer getMaxState() {
        return Collections.max(getStates());
    }
    
    
    public EpsNFA shiftStates(int delta) {
        EpsNFA newNfa = new EpsNFA();
        
        for (int src : trans.keySet())
            for (int dst : trans.get(src).keySet())
                newNfa.addTransitions(src + delta, dst + delta, trans.get(src).get(dst));
        
        for (int acc : accepting)
            newNfa.addAcceptingState(acc + delta);
        
        newNfa.setInitialState(initial + delta);
        
        return newNfa;
    }
    
    
    public Set<Integer> epsClosure(Integer q) {
        List<Integer> toVisit = new ArrayList<Integer>();
        toVisit.add(q);
        
        Set<Integer> closure = new HashSet<Integer>();
        
        while (!toVisit.isEmpty()) {
            Integer p = toVisit.remove(0);
            closure.add(p);
            
            if (trans.containsKey(p))
                for (Integer dst : trans.get(p).keySet())
                    if (trans.get(p).get(dst).contains(EPSILON))
                        if (!closure.contains(dst))
                            toVisit.add(dst);
        }
        
        return closure;
    }
    
}
