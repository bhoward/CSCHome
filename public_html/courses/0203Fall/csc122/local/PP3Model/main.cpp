// main.cpp -- Test client for Poly ADT
// Brian Howard, Fall 2002

#include "poly.h"
#include <iostream>

void displayTerm(double c, int n)
// Print a nice representation of the term cx^n
// Precondition: c >= 0, (the +/- sign has already been printed by the caller)
//   (c == 0 only if n == 0 and this is the sole term in the polynomial)
// Postcondition: a term of the form cx^n has been printed to cout
{
    if (c != 1) cout << c;
    if (n > 0) cout << "x";
    if (n > 1) cout << "^" << n;
}

void displayPoly(const Poly &p)
// Print a nice representation of p on cout
{
    double c;
    int d = p.degree();

    // Print the leading term
    c = p.coefficient(d);
    if (c < 0) {
        cout << "-";
        c = -c;
    }
    displayTerm(c, d);

    // Print the remaining non-zero terms
    while (--d >= 0) {
        c = p.coefficient(d);
        if (c != 0) {
            if (c < 0) {
                cout << " - ";
                c = -c;
            } else {
                cout << " + ";
            }
            displayTerm(c, d);
        }
    }
}

int main() {
    Poly p;

    // set up 4x^5 + 7x^3 - x^2 + 9 in p
    p.changeCoefficient(4, 5);
    p.changeCoefficient(7, 3);
    p.changeCoefficient(-1, 2);
    p.changeCoefficient(9, 0);

    cout << "p is ";
    displayPoly(p);
    cout << endl;

    cout << "The degree of p is " << p.degree() << endl;
    cout << "The leading coefficient of p is "
         << p.coefficient(p.degree()) << endl;

    p.changeCoefficient(8 + p.coefficient(3), 3);

    cout << "After adding 8x^3, p is ";
    displayPoly(p);
    cout << endl;

    Poly q;
    // set up 2x^2 - 3x + 4 in q
    q.changeCoefficient(2, 2);
    q.changeCoefficient(-3, 1);
    q.changeCoefficient(4, 0);

    cout << "q is ";
    displayPoly(q);
    cout << endl;

    Poly r;
    // add p to q and put the result in r
    for (int i = 0; i <= 5; i++) {
        r.changeCoefficient(p.coefficient(i) + q.coefficient(i), i);
    }

    cout << "p + q is ";
    displayPoly(r);
    cout << endl;

    cout << "Hit enter to quit" << endl;
    cin.get();
    return 0;
}
