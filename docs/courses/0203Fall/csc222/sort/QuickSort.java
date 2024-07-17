/**
 * Implementation of a Quicksort per sedgewick C++ third edition 1998
 *
 * @author David Howard
 * 
 */
class QuickSort extends  Sorter {
    public String getName() {
        return "Quick Sort";
    }
    
    // sedgewick C++ third edition 1998
    private int partition(int[] a,int l,int r) {
        int tmp;
        int i = l-1;
        int j = r;
        int v = a[r]; // partition point
        for(;;) {
            // scan up to find first item greater than v
            // won't go past end because v = last item in array
            while(a[++i] < v) {
                count();
            }
            // scan down down to find first item less than v
            // or quit if there are none
            while(v < a[--j]) {
                count();
                if (j == l) break;
            }
            // if scan points cross, quit
            if (i >= j) break;
            
            // exchange the elements
            tmp  = a[i];
            a[i] = a[j];
            a[j] = tmp;
           
            count();
        }
            
        // final swap
        a[r] = a[i];
        a[i] = v;
        
        countUpdate(a);
        return i;
    }            
            
        
    private void qsort(int[] a,int l,int r) {
        count();
        if (r <= l) return;
        int i = partition(a,l,r);
        qsort(a,l,i-1);
        qsort(a,i+1,r);
    }
    
    public void sort(int[] a) {
        init();
        qsort(a,0,a.length-1);
        update(a);
        fin();
    }
}
