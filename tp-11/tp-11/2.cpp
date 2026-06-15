#include "LinkedList.h";

// Devuelve la suma de todos los elementos.
int sumatoria(LinkedList xs) {
    ListIterator it = getIterator(xs);
    int sumadoHastAhora = current(it);
    while (!atEnd(it)) {
        Next(it);
        sumadoHastAhora += current(it);
    };
    DisposeIterator(it);
    return sumadoHastAhora;
};

// Incrementa en uno todos los elementos.
void Sucesores(LinkedList xs) {
    ListIterator it = getIterator(xs);
    while (!atEnd(it)) {
        SetCurrent(current(it)+1, it);
        Next(it);
    };
    DisposeIterator(it);
    SetCurrent(current(it)+1, it);
};

// Indica si el elemento pertenece a la lista.
bool pertenece(int x, LinkedList xs) {
    ListIterator it = getIterator(xs);
    while(current(it) != x && !atEnd(it)) {
        Next(it);
    };
    DisposeIterator(it);
    return current(it) == x;
};

// Indica la cantidad de elementos iguales a x.
int apariciones(int x, LinkedList xs) {
    ListIterator it = getIterator(xs);
    int apariciones = 0;
    while (!atEnd(it)) {
        if (current(it) == x) { apariciones++; };
        Next(it);
    };
    if (current(it) == x) { apariciones++; };
    DisposeIterator(it);
    return apariciones;
};

// Devuelve el elemento mÆs chico de la lista.
int minimo(LinkedList xs) {
    ListIterator it = getIterator(xs);

    int elMinimo = current(it);

    while (!atEnd(it)) {
        Next(it);
        elMinimo = min(elMinimo, current(it));
    };
    DisposeIterator(it);
    return elMinimo;

};

// Dada una lista genera otra con los mismos elementos, en el mismo orden.
// Nota: notar que el costo mejorara si Snoc fuese O(1), cmo podra serlo?
LinkedList copy(LinkedList xs) {
    LinkedList nuevaLista = nil();

    ListIterator itXs = getIterator(xs);
    ListIterator itYs = getIterator(nuevaLista);

    while (!atEnd(itXs)) {
        SetCurrent(current(itXs), itYs);
        Next(itXs); Next(itYs);
    };

    SetCurrent(current(itXs), itYs);
    Next(itXs); Next(itYs);
    
    DisposeIterator(itXs);
    DisposeIterator(itYs);
    
    return nuevaLista;

};

// Agrega todos los elementos de la segunda lista al nal de los de la primera.
// La segunda lista se destruye.
// Nota: notar que el costo mejorara si Snoc fuese O(1), cmo podra serlo?
void Append(LinkedList xs, LinkedList ys) {
    ListIterator itYs = getIterator(ys);
    while (!atEnd(itYs)) { Snoc(current(itYs), xs); Next(itYs); };
    Snoc(current(itYs), xs);    
    DisposeIterator(itYs);
    DestroyL(ys);
};



bool min(int a, int b) { if (a<b) { return a; } else { return b; }; };