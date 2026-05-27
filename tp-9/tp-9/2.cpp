// Precondición: c1 < c2
void printFromTo(char c1, char c2) {
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
}
// Muestra en consola los caracteres desde c1 hasta c2, ambos incluidos.


// Precondición: n >= 0
int fc(int n) {
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
    }
    return x;
}
// Devuelve el factorial de n


// Precondición: n <= m
int ft(int n, int m) {
    if (n == m) {
        return n;
    }
    return n + ft(n+1, m);
}
// Devuelve la suma de los enteros desde n hasta m, ambos incluidos.