/**
 * Implementation of a Heap Sort per sedgewick C++ third edition 1998
 *
 * @author David Howard
 * 
 */
class HeapSort extends  Sorter {
    public String getName() {
        return "Heap Sort";
    }
    
    // sedgewick C++ third edition 1998
    private void fixDown(int[] a,int pq,int k,int N) {
        int tmp;
        while(2*k < N) {
            int j = 2*k;
            if ((j < N)&&(a[pq + j] < a[pq + j + 1])) {
                j++;
                count();
            }
            if (!(a[pq + k] < a[pq + j])) {
                break;
            }
            tmp = a[pq + k];
            a[pq + k] = a[pq + j];
            a[pq + j] = tmp;
            k = j;
            count();
        }
    }
    
    private void heapsort(int[] a,int l,int r) {
        count();
        int k;
        int N  = r-l+1;
        int pq = l-1;
        int tmp;
        
        for(k=N/2;k>=1;k--) {
            fixDown(a,pq,k,N);
            countUpdate(a);
        }
        while(N > 1) {
            tmp = a[pq+N];
            a[pq+N] = a[pq + 1];
            a[pq+1] = tmp;
            count();
            fixDown(a,pq,1,--N);
            countUpdate(a);
        }
    }
    
    public void sort(int[] a) {
        init();
        heapsort(a,0,a.length-1);
        update(a);
        fin();
    }
}
