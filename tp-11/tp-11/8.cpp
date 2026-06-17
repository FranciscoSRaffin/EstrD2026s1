#include "Tree.cpp";

//  Puesto unicamente para no mostrar errores // 
typedef QueueT;
typedef ArrayList;
QueueT emptyQ();
bool isEmptyQ(QueueT q);
Tree firstQ(QueueT q);
void Enqueue(Tree x, QueueT q);
void Dequeue(QueueT q);
int lengthQ(QueueT q);
void MergeQ(QueueT q1, QueueT q2);
void DestroyQ(QueueT q);
// // // // // // // // // // // // // // // //


int sumarT(Tree t) {
    QueueT queue = emptyQ();
    int acc = 0;

    encolarSiNoEsNulo(t, queue);

    while (!isEmptyQ(queue)) {
        Tree ta = firstQ(queue);
        acc += ta->elem;
        encolarSiNoEsNulo(ta->left, queue);
        encolarSiNoEsNulo(ta->left, queue);
        Dequeue(queue);
        delete ta;
    }
    DestroyQ(queue);
    return acc;

};

void encolarSiNoEsNulo(Tree t, QueueT q) {
    if (t != emptyT()) { Enqueue(t,q); }
}



int sizeT(Tree t) {
    QueueT queue = emptyQ();
    int tamanioContabilizado = 0;

    encolarSiNoEsNulo(t, queue);

    while (!isEmptyQ(queue)) {
        tamanioContabilizado++;
        
        Tree nodo = firstQ(queue); Dequeue(queue);
        encolarSiNoEsNulo(nodo->left, queue);
        encolarSiNoEsNulo(nodo->right, queue);
        delete nodo;
    }
    DestroyQ(queue);
    return tamanioContabilizado;

};

bool perteneceT(int e, Tree t) {
    QueueT queue = emptyQ();
    
    encolarSiNoEsNulo(t, queue);

    while (!isEmptyQ(queue)) {
        Tree nodo = firstQ(queue); Dequeue(queue);
        if (nodo->elem == e) {
            DestroyQ(queue);
            return true;
        }
        encolarSiNoEsNulo(nodo->left, queue);
        encolarSiNoEsNulo(nodo->right, queue);
        delete nodo; //  TODO: Ver clase para saber si esto hay que eliminarlo o no
    }
    
    DestroyQ(queue);
    return false;

};

int aparicionesT(int e, Tree t) {
    QueueT queue = emptyQ();
    int contadorDeAp = 0;

    encolarSiNoEsNulo(t, queue);

    while(!isEmptyQ(queue)) {
        Tree nodo = firstQ(queue); Dequeue(queue);

        if (nodo->elem == e) { contadorDeAp++; }

        encolarSiNoEsNulo(nodo->left, queue);
        encolarSiNoEsNulo(nodo->right, queue);
        delete nodo;
    };

    DestroyQ(queue);
    return contadorDeAp;

};

ArrayList toList(Tree t) {
    ArrayList array = newArrayList();
    QueueT queue = emptyQ();

    encolarSiNoEsNulo(t, queue);

    while (!isEmptyQ(queue)) {
        Tree nodo = firstQ(queue); Dequeue(queue);

        add(nodo->elem, array);

        encolarSiNoEsNulo(nodo->left, queue);
        encolarSiNoEsNulo(nodo->right, queue);
        delete nodo;
    };

    DestroyQ(queue);
    return array;
};