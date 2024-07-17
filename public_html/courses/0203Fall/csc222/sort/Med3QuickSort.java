/**
 * Implementation of a Median-of-3 Quicksort per sedgewick C++ third edition 1998
 *
 * @author Brian Howard
 * 
 */
class Med3QuickSort extends  Sorter {
    public String getName() {
        return "Median-of-3 Quick Sort";
    }
    
    // cutoff value for partitions on which we use insertion sort instead of quicksort
    static final int M = 10;
    
    // sedgewick C++ third edition 1998
    private int partition(int[] a,int l,int r) {
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
            // won't go past end because v >= first item in array
            while(v < a[--j]) {
                count();
            }
            // if scan points cross, quit
            if (i >= j) break;
            
            // exchange the elements
            exch(a,i,j);
        }
            
        // final swap
        a[r] = a[i];
        a[i] = v;
        
        countUpdate(a);
        return i;
    }            
    
    private void exch(int[] a,int i,int j) {
        int temp = a[i];
        a[i] = a[j];
        a[j] = temp;
        count();
    }
    
    private void compexch(int[] a,int i,int j) {
        if (a[i] > a[j]) exch(a,i,j);
        count();
    }
            
    private void qsort(int[] a,int l,int r) {
        count();
        if (r-l <= M) return;
        exch(a,(l+r)/2,r-1);
        compexch(a,l,r-1);
        compexch(a,l,r);
        compexch(a,r-1,r);
        int i = partition(a,l+1,r-1);
        qsort(a,l,i-1);
        qsort(a,i+1,r);
    }

    private void isort(int[] a,int l,int r) {
       for(int i=l+1;i<=r;i++) {
            int j = i;
            int x = a[i];
            while((j>=l+1)&&(x < a[j-1])) {
                a[j] = a[j-1];
                j--;
                
                // count every inner loop swap 
                count();
            }
            a[j] = x;
            // count outer loop swaps
            countUpdate(a);
        }
    }
    
    public void sort(int[] a) {
        init();
        qsort(a,0,a.length-1);
        isort(a,0,a.length-1);
        update(a);
        fin();
    }
}
