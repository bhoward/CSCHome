// poly.h -- Interface to the Polynomial ADT
// Brian Howard, Fall 2002

#include "ListA.h"

class Poly {
public:
    Poly();   // will be zero if coeffs list is empty
    int degree() const;
    double coefficient(int power) const;
    void changeCoefficient(double newCoeff, int power);

private:
    List coeffs;
};
