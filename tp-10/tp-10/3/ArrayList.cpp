struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaæo del array
}


typedef ArrayListSt* ArrayList;

ArrayList newArrayList() {
    ArrayListSt* a = new ArrayList;
    a->cantidad = 0;
    a->capacidad = 16;
    a->elementos = new int[a->capacidad];
    return a;
};

ArrayList newArrayListWith(int capacidad) {
    ArrayListSt* a = new ArrayList;
    a->cantidad = 0;
    a->capacidad = capacidad;
    a->elementos = new int[a->capacidad];
    return a;
};

int lengthAL(ArrayList xs) {
    return xs->cantidad;
};

int get(int i, ArrayList xs) {
    if (i<=xs->capacidad){
        return xs->elementos[i-1];
    } return NULL;
};

void set(int i, int x, ArrayList xs) {
    xs->elementos[i-1] = x;
};

void resize(int capacidad, ArrayList xs) {
    xs->capacidad = capacidad;
};

void add(int x, ArrayList xs) {
    if (xs->cantidad == xs->capacidad) {
        newAl = newArrayListWith(xs->capacidad*2)
        for (i=0; i!=xs->cantidad; i++) {
            newAl->elemento[i] = xs->elemento[i];
            delete xs->elemento[i];
        };
        delete xs;
        xs = newAl;
    }
    xs->cantidad++;
    xs->elementos[xs->cantidad] = x;
};

void remove(ArrayList xs) {
    xs->cantidad--
};