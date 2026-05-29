#include <iostream>
#include "Par.h"
using namespace std;

int main() {
    Par p = consPar(3, 5);
    cout << "El primer elemento del par es: " << fst(p) << endl;
    cout << "El segundo elemento del par es: " << snd(p) << endl;
    cout << "El mayor elemento del par es: " << maxDelPar(p) << endl;

    Par p2 = swap(p);
    cout << "El primer elemento del par intercambiado es: " << fst(p2) << endl;
    cout << "El segundo elemento del par intercambiado es: " << snd(p2) << endl;

    int n = 17, m = 5;
    Par dyr = divisionYResto(n, m);
    cout << "La división de " << n << " entre " << m << " es: " << fst(dyr) << endl;
    cout << "El resto de la división de " << n << " entre " << m << " es: " << snd(dyr) << endl;

}