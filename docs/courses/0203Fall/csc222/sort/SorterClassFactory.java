/**
 * Class Factory to get a sort instance
 *
 * @author David Howard
 * @suthor Brian Howard
 * 
 * the author grants use of this code for any purpose with no guarantee that it works
 */
public class SorterClassFactory {
    String className;
    
    public SorterClassFactory(String name) {
        className = name;
    }
    
    public SorterClassFactory() {
    }
    
    public Sorter getInstance() {
        try {
            return (Sorter) Class.forName(className).newInstance();
        } catch(Exception e) {
            return null;
        }
    }
};


