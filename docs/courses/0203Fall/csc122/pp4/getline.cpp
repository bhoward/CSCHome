// Fixed version of non-member getline
// Obtained from Mark Allen Weiss, Florida International University
// http://www.cs.fiu.edu/~weiss/adspc++2/code/

#include "getline.h"

istream & getline( istream & in, string & str, char delim )
{
    char ch;
    str = "";     // empty string, will build one char at-a-time
    
    while( in.get( ch ) && ch != delim )
        str += ch;
    
    return in;
}

istream & getline( istream & in, string & str )
{
    return getline( in, str, '\n' );
}

