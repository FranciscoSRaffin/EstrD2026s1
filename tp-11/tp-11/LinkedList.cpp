#include "LinkedList.h"
struct NodoL;
struct LinkedListSt;
typedef LinkedListSt* LinkedList;
struct IteratorSt;
typedef IteratorSt* ListIterator;

LinkedList nil() {
    LinkedListSt* list = new LinkedListSt;
    list->cantidad = 0;
    list->primero = NULL;
    list->ultimo = NULL;
    return list;
};

bool isEmpty(LinkedList xs) {
    return xs->cantidad == 0;
};

int head(LinkedList xs) {
    return xs->primero->elem;
};

void Cons(int x, LinkedList xs) {
    NodoL* nuevoSegundo = xs->primero; // xs->primero --> NodoL | NULL
    xs->primero = new NodoL;
    xs->primero->elem = x;
    xs->primero->siguiente = nuevoSegundo;
};

void Tail(LinkedList xs) {
    NodoL* primerElemento = xs->primero;
    xs->primero->elem = primerElemento->siguiente->elem;
    delete primerElemento;
    xs->cantidad--;
};

int length(LinkedList xs) {
    return xs->cantidad;
};

void Snoc(int x, LinkedList xs) {
    NodoL* nuevoNodo = new NodoL;
    nuevoNodo->elem = x;
    nuevoNodo->siguiente = NULL;
    if (xs->primero == NULL) { xs->primero = nuevoNodo; } 
    else {
        NodoL* nodoActual = xs->primero;
        while (nodoActual->siguiente != NULL) {
            nodoActual = nodoActual->siguiente;
        }
        nodoActual->siguiente = nuevoNodo;
    };
    xs->cantidad++;
};

ListIterator getIterator(LinkedList xs) {
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs;
};

int current(ListIterator ixs) {
    return ixs->current->elem;
};

void SetCurrent(int x, ListIterator ixs) {
    ixs->current->elem = x;
};

void Next(ListIterator ixs) {
    ixs->current = ixs->current->siguiente;
};

bool atEnd(ListIterator ixs) {
    return ixs->current->siguiente == NULL;
};

void DisposeIterator(ListIterator ixs) {
    delete ixs;
};

void DestroyL(LinkedList xs) {
    if (xs->primero == NULL) {
        delete xs;
    } else {
        NodoL* nodoActual = xs->primero;
        NodoL* nodoADescartar;
        while (nodoActual->siguiente != NULL) {
            nodoADescartar = nodoActual;
            nodoActual = nodoActual->siguiente;
            delete nodoADescartar;
        };
    }
    delete xs;
};

void Append(LinkedList xs,LinkedList ys) {
    xs->ultimo->siguiente = ys->primero;
    delete ys;
};