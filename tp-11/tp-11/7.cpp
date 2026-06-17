int sumarT(Tree t) {
    if isEmptyT(t) { return 0 }
    return rootT(t) + sumarT(left(t)) + sumarT(right(t));
};

int sizeT(Tree t) {
    if isEmptyT(t) { return 0 }; 
    return 1 + sumarT(left(t)) + sumarT(right(t));
};

bool perteneceT(int e, Tree t) {
    if isEmptyT(t) { return false; }
    return (e == rootT(t)) || perteneceT(e, left(t)) || perteneceT(e, right(t));
};

int aparicionesT(int e, Tree t) {
    if isEmptyT(t) { return 0; }
    return delta(rootT(t) == e) + aparicionesT(e, left(t)) + aparicionesT(e, right(t))
};

int delta(bool b) { if b {return 1} else {return 0} };
int max(int a, int b) { if a>b {return a} else {return b} };

int heightT(Tree t) {
    if isEmptyT(t) { return 0; } 
    return 1 + max(heightT(left(t)), heightT(right(t)))
};

ArrayList toList(Tree t) {
    ArrayList newArray = newArrayList();
    if (!isEmptyT(t)) {
        add(rootT(t), newArray);
        append(
            newArray,
            append(toList(left(t)), toList(right(t)))
        );
    };
    return newArray;
};

ArrayList leaves(Tree t) {
    ArrayList newArray = newArrayList();

    if (!isEmptyT(t) && isEmptyT(left(t)) && isEmptyT(right(t))) {
        add(newArray, rootT(t))
    }
    if (!isEmptyT(t) && (!isEmptyT(left(t)) || !isEmptyT(right(t)))) {
        append(newArray, append(leaves(left(t)), leaves(right(t))));
    }
    return newArray;
};

ArrayList levelN(int n, Tree t) {
    ArrayList array = newArrayList();
    
    if(n==0) {
        add(rootT(t), array)
    } else {
        append(array, append(levelN(n-1,left(t)), levelN(n-1,right(t))))
    }

    return array;
};