/**
 * Implementation of a Shell Sort per sedgewick C++ third edition 1998
 *
 * @author David Howard
 * 
 */
class ShellSort extends  Sorter {
    public String getName() {
        return "Shell Sort";
    }
        
    public void sort(int[] a) {
        init();
        int h;
        int l = 0;
        int r = a.length - 1;
        for(h=1;h<=(r-l)/9;h=3*h+1);
        for(;h > 0;h /= 3) {
            for(int i=l+h;i<=r;i++) {
                int j = i;
                int x = a[i];
                while((j>=l+h)&&(x < a[j-h])) {
                    a[j] = a[j-h];
                    j -= h;
                    
                    // count every inner loop swap 
                    countUpdate(a);
                }
                a[j] = x;
                // count outer loop swaps
                countUpdate(a);
            }
            update(a);
        }
        fin();
    }
}
