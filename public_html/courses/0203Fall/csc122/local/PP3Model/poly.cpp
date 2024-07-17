// poly.cpp -- Implementation of Poly ADT

#include "poly.h"

// Utility function to convert the power of a term to a list index
int toIndex(int power) {
    return power + 1;
}

Poly::Poly()
{
    // Initialize the list of coefficients to represent "0"
    bool success;
    coeffs.insert(1, 0, success);
}

int Poly::degree() const
{
    // Coefficient of x^n is stored at position n+1 in coeffs,
    // so degree is one less than the length
    return coeffs.getLength() - 1;
}

double Poly::coefficient(int power) const
{
    bool success;
    double c;
    coeffs.retrieve(toIndex(power), c, success);  // Try to get coefficient
    if (success) return c;                        // Succeeded
    else return 0;                                // Failed, so coeff is 0
}

void Poly::changeCoefficient(double newCoeff, int power)
{
    bool success;
    coeffs.remove(toIndex(power), success);  // Remove old coefficient
    if (!success) {
        // coefficient was not there, so first insert zeroes to fill the gap
        for (int i = degree() + 1; i < power; i++) {
            coeffs.insert(toIndex(i), 0, success);
            // We'll ignore success, because there's nothing we can do if it fails
        }
    }
    // Now insert the new coefficient
    coeffs.insert(toIndex(power), newCoeff, success);

    // The mathematical definition of degree requires that we remove any
    //   leading zeroes from the polynomial
    for (int d = degree(); d > 0 && coefficient(d) == 0; d--) {
        coeffs.remove(toIndex(d), success);
    }
}
