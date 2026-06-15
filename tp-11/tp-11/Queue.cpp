struct NodoQ {
    int elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};
struct QueueSt {
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};
typedef QueueSt* Queue;

Queue emptyQ() {
    QueueSt* nq = new QueueSt;
    nq->cantidad = 0;
    nq->primero = NULL;
    nq->ultimo = NULL;
    return nq;
};

bool isEmptyQ(Queue q) {
    return q->cantidad == 0;
};

int firstQ(Queue q) {
    if (q->primero == NULL) {
        return NULL;
    } else {
        return q->primero->elem;
    }
};

void Enqueue(int x, Queue q) {
    NodoQ* nuevoNodo = new NodoQ;
    nuevoNodo->elem = x;
    nuevoNodo->siguiente = NULL;
    NodoQ* nodoActual = q->primero;
    if (nodoActual == NULL) {
        q->primero = nuevoNodo;
    } else {
        while (nodoActual->siguiente != NULL) {
            nodoActual = nodoActual->siguiente;
        };
        nodoActual->siguiente = nuevoNodo;
    };
    q->ultimo = nuevoNodo;
    q->cantidad++;
};

void Dequeue(Queue q) {
    if (q->cantidad == 0) { return; }
    if (q->cantidad == 1) {
        delete q->primero;
        delete q->ultimo;
    } else {
        NodoQ* segundo = q->primero->siguiente;
        delete q->primero;
        q->primero = segundo;
    };
    q->cantidad--;
};

int lengthQ(Queue q) {
    return q->cantidad;
};

void MergeQ(Queue q1, Queue q2) {
    q1->cantidad += q2->cantidad;
    q1->ultimo->siguiente = q2->primero;
    delete q2;
};

void DestroyQ(Queue q) {
    NodoQ* nodoActual = q->primero;
    delete q;

    while (nodoActual != NULL) {
        NodoQ* siguienteNodo = nodoActual->siguiente;
        delete nodoActual;
        nodoActual = siguienteNodo; 
    };
};
