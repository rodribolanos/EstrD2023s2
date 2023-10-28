#include "Par.h"
#include <iostream>
using namespace std;

Par consPar(int x, int y) {
    Par p;
    p.x = x;
    p.y = y;

    return p;
}

int fst(Par p) {
    return p.x;
}

int snd(Par p) {
    return p.y;
}

int maxDelPar(Par p) {
    if ((p.x) > (p.y)) {
        return p.x;
    } 
        return p.y;
    
}

Par swap(Par p) {
    int newX = p.x;
    p.x = p.y;
    p.y = newX;
    return p;
}

Par divisionYResto(int n, int m) {
    Par p = consPar((n / m), (n % 3));
    return p;
}