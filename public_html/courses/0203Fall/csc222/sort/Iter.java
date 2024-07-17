/**
 * The class for counting 'operations' within an algorithm
 * and for triggering repaints after a given number of operations
 * contained within a bounded range of values.
 *
 * @author David Howard
 * 
 * the author grants use of this code for any purpose with no guarantee that it works
 */
public class Iter {
    public static final long limit = 50;
    private long xcount;
    private long operations;

    public Iter() {
        xcount     = 0;
        operations = 0;
    }

    public long getCount() {
        return operations;
    }
    
    public boolean test() {
        boolean result;
        
        count();
        if (xcount > limit) {
            xcount = 0;
            result =  true;
        }
        else {
            result = false;
        }
        return result;
    }
    
    public void count() {
        operations++;
        xcount++;
    }
}
