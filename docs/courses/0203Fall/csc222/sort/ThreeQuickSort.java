/**
 * Implementation of a Three-Way Partitioning Quicksort per sedgewick C++ third edition 1998
 *
 * @author Brian Howard
 * 
 */
class ThreeQuickSort extends  Sorter {
    public String getName() {
        return "Three-Way Partitioning Quick Sort";
    }
    
    // cutoff value for partitions on which we use insertion sort instead of quicksort
    static final int M = 10;

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
        int i = l;
        int j = r-1;
        int p = i;
        int q = j;
        int v = a[j]; // partition point
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
            
            // move elements equal to the pivot out to the sides
            count();
            if (a[i] == v) {
                p++;
                exch(a,p,i);
            }
            
            count();
            if (a[j] == v) {
                q--;
                exch(a,q,j);
            }
        }
            
        // swap the pivot into position
        a[r-1] = a[i];
        a[i] = v;
        countUpdate(a);
                
        // move the duplicate items to the region around the pivot
        j = i-1;
        i = i+1;
        for (int k = l+1; k <= p; k++, j--) exch(a,k,j);
        for (int k = r-2; k >= q; k--, i++) exch(a,k,i);

        qsort(a,l,j);
        qsort(a,i,r);
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
