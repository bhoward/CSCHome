/**
 * Implementation of a Merge Sort per sedgewick C++ third edition 1998
 *
 * @author David Howard
 * 
 */
class MergeSort extends  Sorter {
    int[] aux;

    public String getName() {
        return "Merge Sort";
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
    }

    private void mergesort(int[] a,int l,int r) {
        count();
        if (r <= l) return;
        int m = (r+l)/2;
        mergesort(a,l,m);
        mergesort(a,m+1,r);
        merge(a,l,m,r);
        // count every movement of elements
        countUpdate(a);
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
