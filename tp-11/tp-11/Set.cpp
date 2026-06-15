#include "Set.h";

struct NodoS;
struct SetSt;
typedef SetSt* Set;

Set emptyS() {
    SetSt* set = new SetSt;
    set->cantidad = 0;
    set->primero = NULL; 
    return set;

};

bool isEmptyS(Set s) {
    return s->cantidad == 0;
};

bool belongsS(int x, Set s) {    
    NodoS* nodoActual = s->primero;

    while (s->cantidad != 0 && nodoActual->siguiente != NULL) {
        if (nodoActual->elem == x) {
            return true;
        };
        nodoActual = nodoActual->siguiente; 
    }; return false;
    
};

void AddS(int x, Set s) {
    
    if (s->cantidad == 0) {
        NodoS* nodoNuevo = new NodoS;
        nodoNuevo->elem = x; nodoNuevo->siguiente = NULL;
        s->primero = nodoNuevo; s->cantidad++;
    } else {
        NodoS* nodoActual = s->primero;
        while (nodoActual->siguiente != NULL) {
            if (nodoActual->elem == x) { return; };
            nodoActual= nodoActual->siguiente;
        };
        NodoS* nodoNuevo = new NodoS;
        nodoNuevo->elem = x; nodoNuevo->siguiente = NULL;
        nodoActual->siguiente = nodoNuevo; s->cantidad++;
    };

};

void RemoveS(int x, Set s) {
    // MEJORABLE USANDO (nodoActual != NULL)
    if (s->cantidad == 0) { return; }

    NodoS* nodoActual = s->primero;
    if (nodoActual->elem == x) {
        delete nodoActual;
        s->primero = NULL;
        s->cantidad--;
    } else {
        while (nodoActual->siguiente != NULL && nodoActual->siguiente->elem != x) {
            nodoActual = nodoActual->siguiente;
        };
        if (nodoActual->siguiente != NULL && nodoActual->siguiente->elem == x) {
            delete nodoActual->siguiente;
            nodoActual->siguiente = NULL;
            s->cantidad--;
        }
    }
};

int sizeS(Set s) { return s->cantidad; };

LinkedList setToList(Set s) {
    LinkedList nuevaLista = nil();
    NodoS* nodoActual = s->primero;
    delete s;
    while (nodoActual != NULL) {
        Cons(nodoActual->elem, nuevaLista);
        NodoS* nuevoSiguiente = nodoActual->siguiente;
        delete nodoActual;
        nodoActual = nuevoSiguiente;
    };
    return nuevaLista;
};

void DestroyS(Set s) {
    NodoS* nodoActual = s->primero;
    delete s;
    while (nodoActual != NULL) {
        NodoS* nodoSig = nodoActual->siguiente;
        delete nodoActual;
        nodoActual = nodoSig;
    };
};
