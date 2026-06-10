#include ArrayList;

int sumatoria(ArrayList xs) {
    int acumulador = 0;
    for (i=1; i<=lengthAL(xs) ;i++){
        acumulador += get(i,xs);
    }; return acumulador;
}

void sucesores(ArrayList xs) {
    for (i=1; i<=lengthAL(xs) ;i++){
        set(i, get(i, xs), xs);
    };
};

bool pertenece(int x, ArrayList xs) {
    for (i=1; i<=lengthAL(xs) ;i++){
        if (x == get(i, xs)){ // Evito recursión por optimización de memoria
            return true;
        };
    }; return false;
};

int apariciones(int x, ArrayList xs) {
    int vistosHastaAhora = 0;
    for (i=1; i<=lengthAL(xs) ;i++){
        if (x == get(i, xs)){
            vistosHastaAhora++;
        };
    }; return vistosHastaAhora;

};


ArrayList append(ArrayList xs, ArrayList ys) {
    newArray = xs;
    for (i=1; i<=lengthAL(ys) ;i++) {
        // El resize no es necesario ya que esto se gestiona dentro del add
        add(get(i, ys), newArray);
    };
    return newArray;
};

int minimo(ArrayList xs) {
    int menorHastaAhora = get(1, xs);
    for (i=2; i<=lengthAL(xs) ;i++){
        menorHastaAhora = min(get(i, xs), menorHastaAhora);
    };
    return menorHastaAhora;
}
