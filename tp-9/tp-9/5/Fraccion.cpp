#include "Fraccion.h"

Fraccion consFraccion(int numerador, int denominador) {
    Fraccion f;
    f.numerador = numerador; 
    f.denominador = denominador; 
    return f
};

int numerador(Fraccion f) {
    return f.numerador
};

int denominador(Fraccion f){
    return f.denominador
};

float division(Fraccion f) {
    return f.numerador / f.denominador
};

Fraccion multF(Fraccion f1, Fraccion f2) {
    f1.denominador = f1.denominador * f2.denominador
    f1.numerador = f1.numerador * f2.numerador
    return f1
};

Fraccion simplificada(Fraccion p) {
    int d = mcd(p.numerador, p.denominador);

    p.numerador /= d;
    p.denominador /= d;

    return p;
}

int mcd(int a, int b) {
    a = abs(a);
    b = abs(b);

    while (b != 0) {
        int r = a % b;
        a = b;
        b = r;
    }

    return a;
}

Fraccion sumF(Fraccion f1, Fraccion f2) {
    f1.numerador = (f1.numerador * f2.denominador) + (f1.denominador * f2.numerador);
    f1.denominador = f1.denominador * f2.denominador;

    return simplificada(f1);
};