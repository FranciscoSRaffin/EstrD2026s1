
// Iterativa
void printN(int n, string s) {
    while (n > 0) {
        cout s << endl;
        n--
    }
}

void cuentaRegresiva(int n) {
    while (n > 0) {
        cout n << endl;
        n--
    }
}

void desdeCeroHastaN(int n) {
    int x = 0;

    while (n !== x) {
        cout x << endl;
        x++
    }
}

int mult(int n, int m) {
    while (n > 0) {
        m = m + m
    }
}

void primerosN(int n, string s) {
    for (int i = 0; i < n; i++) {
        cout s << endl;
    }
    
}

bool pertenece(char c, string s) {
    for (int i = 0; i < s.length(); i++) {
        if (s[i] == c) {
            return true;
        }
    }
    return false;
}



int apariciones(char c, string s) {
    int count = 0;
    for (int i = 0; i < s.length(); i++) {
        if (s[i] == c) {
            count++;
        }
    }
    return count;

}



// Recursiva
void printN(int n, string s) {
    if (n > 0) {
        cout s << endl;
        printN(n-1, s);
    }  
}

void cuentaRegresiva(int n) {
    if (n > 0) {
        cout n << endl;
        cuentaRegresiva(n-1);
    }
}

void desdeCeroHastaN(int n) {
    int x = 0;
    desdeHasta(x, n);
}


desdeHasta(int x,int n) {
    if (x != n) {
        cout x << endl;
        desdeHasta(x+1,n)
    }
}


int mult(int n, int m) {
    if (m == 1){
        return (n);
    } else {
        return (n + mult(n, m-1));
    } 

}

void primerosN(int n, string s) {
    
    if (n==0 && s) {
        cout s[0];
        primerosN(n, s.substr(1));
    } else {
        primerosN(n-1, s.substr(1));
    }
    
}

bool pertenece(char c, string s) {
    return (s[0] == c || pertenece(c, s.substr(1)));
}

int apariciones(char c, string s) {
    int delta = (s[0] == c) ? 1 : 0;
    return delta + apariciones(c, s.substr(1));
}
