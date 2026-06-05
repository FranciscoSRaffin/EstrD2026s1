struct PokeSt;
typedef PokeSt* Pokemon;

typedef string TipoDePokemon;


Pokemon consPokemon(TipoDePokemon tipo) {
    Pokemon p = new Pokemon;

    p->tipo = tipo;
    p->vida = 100;

    return p
};

TipoDePokemon tipoDePokemon(Pokemon p) {
    return p->tipo
};

int energia(Pokemon p) {
    return p->vida
};

void perderEnergia(int energia, Pokemon p) {
    p->vida -= energia;
};

bool superaA(Pokemon p1, Pokemon p2) {
    TipoDePokemon t1 = p1->tipo
    TipoDePokemon t2 = p2->tipo

    return (
        (t1 == "agua" && t2 == "fuego") ||
        (t1 == "fuego" && t2 == "planta") ||
        (t1 == "planta" && t2 == "agua")
    );   
};
