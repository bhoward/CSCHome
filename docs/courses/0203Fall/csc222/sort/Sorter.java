import java.util.*;

/**
 * Template for a sort method used by SortCanvas
 *
 * @author David Howard
 * 
 * the author grants use of this code for any purpose with no guarantee that it works
 */
public abstract class Sorter extends Observable {
    abstract public void sort(int[] a);
    
    private Iter iter;
    public long getOperations() {
        long result = 0;
        if (iter != null) {
            result = iter.getCount();
        }
        return result;    
    }
    
    void init() {
        iter = new Iter();
    }
    
    void count() {
        iter.count();
    }
    
    void update(int[] a) {
        setChanged();
        notifyObservers(a.clone());
    }
    
    void fin() {
        setChanged();
        notifyObservers(null);
    }
    
    void countUpdate(int[] a) {
        if (iter.test()) update(a);
    }
    
    abstract public String getName();
};

