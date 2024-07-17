/**
 * Implementation of a Bottom-Up Merge Sort per sedgewick C++ third edition 1998
 *
 * @author Brian Howard
 * 
 */
class BUMergeSort extends  Sorter {
    int[] aux;

    public String getName() {
        return "Bottom-Up Merge Sort";
    }

    
    private void merge(int[] a, int l, int m, int r)  {
        int i;
        int j;

        for (i = m+1; i > l; i--) {
            aux[i-1] = a[i-1];
            // count every movement of elements
            count();
        }

        for (j = m; j < r; j++) {
            aux[r+m-j] = a[j+1];
            // count every movement of elements
            count();
        }

        for (int k = l; k <= r; k++) {
            if (aux[j] < aux[i]) {
                a[k] = aux[j--];
                }
            else {
                a[k] = aux[i++];
            }
            count();
        }
        countUpdate(a);
    }

    private void mergesort(int[] a,int l,int r) {
        for (int m = 1; m <= r-l; m = m+m)
            for (int i = l; i <= r-m; i += m+m)
                merge(a,i,i+m-1,Math.min(i+m+m-1,r));
    }

    public void sort(int[] a) {
        aux  = new int[a.length];
        init();
        
        // sort
        mergesort(a,0,a.length-1);
        
        // signal finished
        fin();
    }
}
