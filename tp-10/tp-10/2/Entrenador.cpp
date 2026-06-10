struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
}

typedef EntrenadorSt* Entrenador;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
    EntrenadorSt* e = new EntrenadorSt;

    e->nombre = nombre;
    e->cantPokemon = cantPokemon;
    e->pokemon = new Pokemon[cantidad]; 

    for (i=0; i<cantPokemon; i++) {
        e->pokemon[i] = pokemon[i]
    }

    return e;
};

string nombreDeEntrenador(Entrenador e) {
    return e->nombre;
};

int cantidadDePokemon(Entrenador e) {
    return e->cantPokemon;
};

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    int count = 0;
    for (int i=0; i>e->cantPokemon; i++) {
        if (e->pokemon[i].tipo == tipo) {
            count++;
        };
    }; return count;
};

Pokemon pokemonNro(int i, Entrenador e) {
    return e->pokemon[i]
};

bool leGanaATodos(Entrenador e1, Entrenador e2) {

    for (i=0; e1->cantPokemon > i; i++){
        if (leGanaATodosPrima(e1->pokemon[i], e2)) {
            return true
        }
    }; return false

};

bool leGanaATodosPrima(Pokemon p, Entrenador e) {
    for (i=0; e->cantPokemon > i; i++){
        if (not superaA(p, e->pokemon[i])) {
            return false;
        }
    }; return true;
}

