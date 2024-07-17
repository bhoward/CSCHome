/**
 * Implementation of a Bubble Sort per sedgewick C++ third edition 1998
 *
 * @author David Howard
 * @author Brian Howard
 * 
 */
class BubbleSort extends  Sorter {
    public String getName() {
        return "Bubble Sort";
    }
    
    public void sort(int[] a) {
        init();
        // sort, with an update to the view on each pass
        update(a);
        for(int i=0;i<a.length-1;i++) {
            for(int j=a.length-1;j > i;j--) {
                count();
                if (a[j] < a[j-1]) {
                    int x  = a[j];
                    a[j  ] = a[j-1];
                    a[j-1] = x;
                    count();
                }
            }
            update(a);
        }
        fin();
    }
}
