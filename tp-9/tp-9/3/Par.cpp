#include "Par.h"

Par consPar(int x, int y) {
    Par par;
    par.x = x; par.y = y;
    return(par);
}


int fst(Par p) {
    return(p.x);
}


int snd(Par p) {
    return(p.y);
}


int maxDelPar(Par p) {
    if (p.x > p.y) {
        return (p.x)
    } else {
        return (p.y)
    }
}


Par swap(Par p) {
    Par p2;

    p2.x = p.y;
    p2.y = p.x;

    return (p2)
}


Par divisionYResto(int n, int m) {
    Par dyr;
    
    dyr.x = n%m;
    dyr.y = n/m;

    return (dyr)
}

