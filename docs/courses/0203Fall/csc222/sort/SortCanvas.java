import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.util.*;
import java.lang.Thread.*;

/**
 * Component used to display results of a sort. Extends a canvas component
 * and may be inserted in any container
 *
 * @author David Howard
 * 
 * the author grants use of this code for any purpose with no guarantee that it works
 */
public class SortCanvas extends Canvas implements Observer {
    public static final int MaxElements = 512;
    public static final int Iterations  = 50;
    private boolean     Busy;
    private boolean     LastUpdate;
    private Sorter      SortMethod;
    private FinishBuild Finished;
    private int[]       InputData;
    private int[]       CurrentArray;
    private long        StepTimeout;
    private Stepper     Stpr;
    private Builder     BuildThread;
    private long        IterCount;
    private String      SortName;
    private Date        StartTime;
    private Date        CurrentTime;
    
    public SortCanvas() {
        LastUpdate  = false;
        Busy        = false;
        SortMethod  = null;
        StepTimeout = 50;
        Stpr        = new Stepper();
    }
    
    public void setSort(Sorter s) {
        SortMethod = s;
        SortMethod.addObserver(this);
    }
    
    public Sorter getSort() {
        return SortMethod;
    }

    // set the current data set to a new set of values
    // input data is set by the parent applet which could execute
    // in the context of another thread
    public synchronized void setData(int[] v) {
        // parse integer elements from the string and insert in the vector
        if (v != null) {
            // make a copy, don't know what caller will do with the data 
            InputData    = (int[])v.clone();
            setCurrentArray(v);
            repaint();
        }
        else {
            InputData = null;
        }
    }
    
    public synchronized int[] getData() {
        int[] x = null;
        // make a copy because it will be sorted
        if (InputData != null) {
            x = (int[])InputData.clone();
        }
        return x;
    }
    
    // current array is set by 'update' method which executes
    // in the context of another thread
    public synchronized int[] getCurrentArray() {
        int[] v;
        if (CurrentArray != null) {
            v = (int[])CurrentArray.clone();
        }
        else {
            v = null;
        }
        return v;
    }
            
    public synchronized void setCurrentArray(int[] a) {
        // input must already be a copy so don't need to clone it
        CurrentArray = a;
    }
    
	public void sortArray() {
	    getSort().sort(getData());
    }
    
    // set the step timeout
    public void setTimeout(long t) {
        StepTimeout = t;
    }
    
    // get the step timeout
    public long getTimeout() {
        return StepTimeout;
    }
    
    // no thread
    public void clrThread() {
        BuildThread = null;
    }

    public void setLastUpdate(boolean b) {
        LastUpdate = b;
    }
    
    public boolean getLastUpdate() {
        return LastUpdate;
    }

    // builder thread sorts in the background
    // this allows other user interface updates while it is being built
    // rather than having to wait. the sorter signals via the 
    // update method of the observer interface when a repaint is required
    class Builder extends Thread {
        public void run() {
            IterCount = 0;
            
            // get the start time
            StartTime = new Date();
            
            // execute the sort
            sortArray();
            
            // when done, mark thread not used
            clrThread();
            
            // signal finished
            Finished.signalDone();
        }
    }

    class FinishBuild extends Observable {
        public void signalDone() {
            setChanged();
            notifyObservers();
        }
    }
    
    public synchronized void redraw(Observer o) {
        // set up an observable to signal finished
        Finished = new FinishBuild();
        Finished.addObserver(o);
        
        // run the builder thread
        if (BuildThread == null) {
            BuildThread = new Builder();
            BuildThread.start();
        }
    }
    
    public synchronized void reset() {
        // run the builder thread
        if (BuildThread == null) {
            setTimeout(0);
            BuildThread = new Builder();
            BuildThread.start();
        }
    }
    
    public boolean step() {
        // signal
        Stpr.signalStep();
        return getLastUpdate();
    }

    class Stepper {
        boolean s;
        
        public Stepper() {
            s = false;
        }
        
        public synchronized void waitStep() {
            try {
                if (getTimeout() == 0) {
                    // wait until signaled
                    while(s == false) {
                        wait();
                    }
                    s = false;
                }
                else {
                    java.lang.Thread.sleep(getTimeout());
                }
            } catch (InterruptedException e) {
            }
        }
        
        // signal one step to whoever is waiting
        public synchronized void signalStep() {
            s = true;
            notifyAll();
        }
    }


    public void plotPoint(Graphics g,int i,int ai,Dimension d,int xo,int yo) {
        g.fillRect(            ((i  * d.width ) / MaxElements) + xo,
                   d.height - (((ai * d.height) / MaxElements) - yo),
                   1,
                   1);
    }
    
    public void update(Graphics g) {
        g.setColor(getForeground());
        paint(g);
    }
    
    public void paint(Graphics g) {
        Dimension d = getSize();
        int[]     ca;
        int       xoffset;
        int       yoffset;
        Image     memImage = createImage(d.width,d.height);
        Graphics  memG     = memImage.getGraphics();
        
        if (SortName != null) {
            memG.drawString("Type  : " + SortName,5,15);
        }
        if (SortMethod != null) {
            memG.drawString("Ops   : " + Long.toString(SortMethod.getOperations()),5,30);
        }
            
        ca = getCurrentArray();
        if (ca != null) {
            // pick whichever axis is smaller and make it square and centered
            if (d.width < d.height) {
                // use truncated height
                yoffset  = (d.height - d.width) / 2;
                // use full width
                d.height = d.width;
                xoffset  = 0;
            }
            else {
                // use truncated width
                xoffset  = (d.width - d.height) / 2;
                // use full height
                d.width  = d.height;
                yoffset  = 0;
            }
            
            for(int i=ca.length-1;i>=0;i--) {
                plotPoint(memG,i,ca[i],d,xoffset,yoffset);
            }
        }
        else {
            System.out.println("CurrentArray is null!");
        }

        // manually dispose the memory graphics context
        memG.dispose();
        
        // blit the image
        g.drawImage(memImage,0,0,this);
        
        // manually flush resources used by the image
        memImage.flush();
    }

    // when the tree changes
    public void update(Observable o,Object arg) {
        if (arg != null) {
            // input arg is a clone of the current state of the array
            setCurrentArray((int[])arg);
            IterCount    = SortMethod.getOperations();
            SortName     = SortMethod.getName();
            setLastUpdate(false);
        }
        else {
            // done
            setLastUpdate(true);
        }
        
        // force a repaint
        repaint();
        
        // wait for user to observe results
        Stpr.waitStep();
    }
}

