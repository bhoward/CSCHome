import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.util.*;
import java.lang.Thread;

/**
 * Main Applet
 *
 * @author David Howard
 * @author Brian Howard
 * 
 * the author grants use of this code for any purpose with no guarantee that it works
 */
public class SortApplet extends Applet implements Observer {
    static final String[] sortName =
        {"Bubble", "Insert", "Select", "Shell", "Heap", "Quick", "Med3Quick", "ThreeQuick", "Merge", "BUMerge"};
    static final int numSorts = sortName.length;
    static final int sortButtonRows = 2;
    
    static final String[] dataName =
        {"Random", "Ordered", "Reverse", "Gaussian", "Limited", "NearOrdered", "NearReverse", "Equal"};
    static final int numData = dataName.length;
    static final int dataButtonRows = 2;
    
    SortCanvas  tp;
    int         step_state;
    Button[]    sortButton;
    Button[]    dataButton;
    int[]       DataVector;
    Object      dvSync;
    
    public SortApplet() {
        tp         = null;
        step_state = 0;
        dvSync     = new Object();
        DataVector = (new RandomDataBuilder()).getDataVector();
    }
    
    public void init()
    {
        Label x;
        
        // applet is border layout
        setLayout(new BorderLayout());
        
        // build the panel that the graph is drawn on
        tp     = new SortCanvas();
        
        // add the panel to the applet
        add(tp,"Center");
        
        // build the bottom row of buttons
        sortButton = new Button[numSorts];
        Panel p = new Panel();
        p.setLayout(new GridLayout(sortButtonRows,-(-numSorts/sortButtonRows)));
        for (int i = 0; i < numSorts; i++) {
            sortButton[i] = new Button(sortName[i]);
            sortButton[i].addActionListener(new SortButtonHandler(new SorterClassFactory(sortName[i] + "Sort"),75));
            p.add(sortButton[i]);
        }
        add(p,"South");
        
        // build the top row of buttons
        dataButton = new Button[numData];
        Panel r = new Panel();
        r.setLayout(new GridLayout(dataButtonRows,-(-numData/dataButtonRows)));
        for (int i = 0; i < numData; i++) {
            dataButton[i] = new Button(dataName[i]);
            DataBuilder d;
            try {
                d = (DataBuilder)Class.forName(dataName[i] + "DataBuilder").newInstance();
            } catch (Exception e) {
                d = null;
            }
            dataButton[i].addActionListener(new DataButtonHandler(d));
            r.add(dataButton[i]);
        }
        add(r,"North");
        
        // start with sort buttons disabled and data buttons enabled
        for (int i = 0; i < numSorts; i++) sortButton[i].setEnabled(false);
        for (int i = 0; i < numData; i++) dataButton[i].setEnabled(true);
    }

    // copy the current data vector
    public int[] getDataVector() {
        int[] x;
        synchronized(dvSync) {
            x = (int[])DataVector.clone();
        }
        return x;
    }

    public void disableButtons() {
        for (int i = 0; i < numSorts; i++) sortButton[i].setEnabled(false);
        for (int i = 0; i < numData; i++) dataButton[i].setEnabled(false);
    }
    
    public void enableButtons() {
        for (int i = 0; i < numSorts; i++) sortButton[i].setEnabled(true);
        for (int i = 0; i < numData; i++) dataButton[i].setEnabled(true);
    }
    
    // fired when the sort canvas finishes sorting
    public void update(Observable o,Object arg) {
        enableButtons();
    }

    // generic sort button handler
    // uses sort class factory to get a sort technique
    class SortButtonHandler implements ActionListener {
        SorterClassFactory SortCF;
        private long       timeout;
        
        public  SortButtonHandler(SorterClassFactory scf,long t) {
            SortCF  = scf;
            timeout = t;
        }
        
        public void actionPerformed(ActionEvent e) {
            disableButtons();
            tp.setTimeout(timeout);
            tp.setData(getDataVector());
            tp.setSort(SortCF.getInstance());
            tp.redraw(SortApplet.this);
        }
    }

    // generic data button handler
    // uses a data vector factory to get a new vector
    class DataButtonHandler implements ActionListener {
        DataBuilder bDB;
        
        public  DataButtonHandler(DataBuilder db) {
            bDB = db;
        }
        
        public void actionPerformed(ActionEvent e) {
            int[] x    = bDB.getDataVector();
            synchronized (dvSync) {
                DataVector = x;
            }
            enableButtons();
            tp.setData(getDataVector());
        }
    }

    public void start() {
    }
}

// the following internal classes are used to build various data vectors
abstract class DataBuilder {
    abstract public int[] getDataVector();
}

// random equally distributed, no duplicates
class RandomDataBuilder extends DataBuilder {
 
    public int[] getDataVector() {
        int       limit = SortCanvas.MaxElements;
        int[]     v    = new int[limit];
        Random    rand = new Random();
        
        for (int i = 0; i < limit; i++) {
            v[i] = i;
        }
        
        for (int i = 0; i < limit-1; i++) {
            int j = rand.nextInt(limit-i) + i;
            int temp = v[i];
            v[i] = v[j];
            v[j] = temp;
        }
        
        return v;
    }
}

// sorted in ascending order, no duplicates
class OrderedDataBuilder extends DataBuilder {
 
    public int[] getDataVector() {
        int       limit = SortCanvas.MaxElements;
        int[]     v     = new int[limit];
        
        for(int j=0;j<v.length;j++) {
            v[j] = j;
        }
        
        return v;
    }
}

// sorted in descending order, no duplicates
class ReverseDataBuilder extends DataBuilder {
    public int[] getDataVector() {
        int       limit = SortCanvas.MaxElements;
        int[]     v    = new int[limit];
        int       i;
        
        i = v.length - 1;
        for(int j=0;j<v.length;j++) {
            v[j] = i;
            i--;
        }
        
        return v;
    }
}

// gaussian with duplicates allowed
class GaussianDataBuilder extends DataBuilder {
 
    public int[] getDataVector() {
        int     limit  = SortCanvas.MaxElements;
        int[]   v      = new int[limit];
        Random  rand   = new Random();
        int     i;
        double  center = (double)limit / 2.0;
        i = 0;
        while(i<v.length) {
            double d = center + (rand.nextGaussian() * 100);
            int    x = Math.abs((int)d);
            if (x < limit) {
                v[i] = x;
                i++;
            }
        }
            
        return v;
    }
}
    
// random equally distributed from a limited range (only 10 values)
class LimitedDataBuilder extends DataBuilder {
 
    public int[] getDataVector() {
        int       limit = SortCanvas.MaxElements;
        int       skip = limit / 10;
        int[]     v    = new int[limit];
        Random    rand = new Random();
        
        for (int i = 0; i < limit; i++) {
            v[i] = (i % 10) * skip;
        }
        
        for (int i = 0; i < limit-1; i++) {
            int j = rand.nextInt(limit-i) + i;
            int temp = v[i];
            v[i] = v[j];
            v[j] = temp;
        }
        
        return v;
    }
}

// nearly sorted in ascending order, no duplicates
class NearOrderedDataBuilder extends DataBuilder {
 
    public int[] getDataVector() {
        int       limit = SortCanvas.MaxElements;
        int[]     v     = new int[limit];
        Random    rand  = new Random();
        
        for(int j=0;j<v.length;j++) {
            v[j] = j;
        }
        
        for (int i = 0; i < limit-10; i++) {
            int j = rand.nextInt(11) + i;
            int temp = v[i];
            v[i] = v[j];
            v[j] = temp;
        }
        
        return v;
    }
}

// nearly sorted in descending order, no duplicates
class NearReverseDataBuilder extends DataBuilder {
    
    public int[] getDataVector() {
        int       limit = SortCanvas.MaxElements;
        int[]     v    = new int[limit];
        Random    rand  = new Random();
        
        for(int j=0;j<limit;j++) {
            v[j] = limit - 1 - j;
        }
        
        for (int i = 0; i < limit-10; i++) {
            int j = rand.nextInt(11) + i;
            int temp = v[i];
            v[i] = v[j];
            v[j] = temp;
        }
        
        return v;
    }
}

// all elements equal
class EqualDataBuilder extends DataBuilder {
 
    public int[] getDataVector() {
        int     limit  = SortCanvas.MaxElements;
        int[]   v      = new int[limit];
        int     n      = limit / 2;
        
        for (int i = 0; i < limit; i++) {
            v[i] = n;
        }
            
        return v;
    }
}
