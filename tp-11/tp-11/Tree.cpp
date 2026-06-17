#include "Tree.h";

struct NodeT {
    int elem;
    NodeT* left;
    NodeT* right;
};

typedef NodeT* Tree;

Tree emptyT() {
    return NULL;
};
Tree nodeT(int elem, Tree left, Tree right) {
    NodeT* n = new NodeT;
    n->elem = elem;
    n->left = emptyT();
    n->right = emptyT();
    return n;
};

bool isEmptyT(Tree t) {
    return t == emptyT();
};

int rootT(Tree t) {
    return t->elem;
};

Tree left(Tree t) {
    return t->left;
};

Tree right(Tree t) {
    return t->right;
};
