/**
 * Implementation of a Selection Sort per sedgewick C++ third edition 1998
 *
 * @author Brian Howard
 * 
 */
class SelectSort extends  Sorter {
    public String getName() {
        return "Selection Sort";
    }
        
    public void sort(int[] a) {
        init();
        int l = 0;
        int r = a.length - 1;
        for(int i = l; i < r; i++) {
            int min = i;
            for (int j = i+1; j <= r; j++) {
                if (a[j] < a[min]) min = j;
                // count every inner loop comparison 
                count();
            }
            int x = a[i];
            a[i] = a[min];
            a[min] = x;
            // count outer loop swaps
            countUpdate(a);
        }
        update(a);
        fin();
    }
}
