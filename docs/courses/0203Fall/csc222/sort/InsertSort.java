/**
 * Implementation of an Insertion Sort per sedgewick C++ third edition 1998
 *
 * @author Brian Howard
 * 
 */
class InsertSort extends  Sorter {
    public String getName() {
        return "Insertion Sort";
    }
        
    public void sort(int[] a) {
        init();
        int l = 0;
        int r = a.length - 1;
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
        update(a);
        fin();
    }
}
