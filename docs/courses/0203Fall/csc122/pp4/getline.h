// Fixed version of non-member getline
// Obtained from Mark Allen Weiss, Florida International University
// http://www.cs.fiu.edu/~weiss/adspc++2/code/

#ifndef SAFE_STL

#include <string>
#ifdef USE_DOT_H
    #include <iostream.h>
#else
    #include <iostream>
    using namespace std;
#endif


#if !defined( __BORLANDC__ ) || __BORLANDC__ < 0x0530

        
// The recent compilers don't seem to have this working correctly.
// So here's my own. Use this on Borland 5.0 and Visual.
// You can remove these lines of code when the compilers catch up.
istream & getline( istream & in, string & str, char delim );

istream & getline( istream & in, string & str );

#endif

#endif
