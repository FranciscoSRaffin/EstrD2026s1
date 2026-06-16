//  Dado un árbol binario de enteros devuelve la suma entre sus elementos.
int sumarT(Tree t) {
    if (t == NULL) {
        return 0 
    } else {
        return t->elem + sumarT(t->left) + sumarT(t->right);
    };
};

//  Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
//en inglés).
int sizeT(Tree t) {
    if (t == NULL) {
        return 0 
    } else {
        return 1 + sumarT(t->left) + sumarT(t->right);
    }; 
};

//  Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
// árbol.
bool perteneceT(int e, Tree t) {
    if (t == NULL) {
        return  
    } else {
        return (e == t->elem)||
               perteneceT(e, t->left) ||
               perteneceT(e, t->right)
        ;
    }; 
};

//  Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
// iguales a e.
int aparicionesT(int e, Tree t) {
    if (t == NULL) {
        return 0;
    } else {
        delta(t->elem == e) + aparicionesT(e, t->left) + aparicionesT(e, t->right)
    }
};

int delta(bool b) { if b {return 1} else {return 0} };
int max(int a, int b) { if a>b {return a} else {return b} };

//  Dado un árbol devuelve su altura.
int heightT(Tree t) {
    if (t == NULL) {
        return 0;
    } else {
        1 + max(heightT(t->left), heightT(t->right))
    }
};

//  Dado un árbol devuelve una lista con todos sus elementos.
ArrayList toList(Tree t) {
    ArrayList newArray = newArrayList();
    
    if (t == NULL) {
        return newArray;
    } else {
        append(add(t->elem, newArray), (
            append(toList(t->left), toList(t->right))
        ))
    };


};

//  Dado un árbol devuelve los elementos que se encuentran en sus hojas.
ArrayList leaves(Tree t) {

};

//  Dados un número n y un árbol devuelve una lista con los nodos de nivel n.
ArrayList levelN(int n, Tree t) {

};