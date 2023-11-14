using namespace std;
#include <iostream>
#include "LinkedList.h"

int sumatoria(LinkedList xs) {
    ListIterator it = getIterator(xs);
    int amount = 0;
    while (not atEnd(it)) {
        amount += current(it);
        next(it);
    }
    return amount;
    DisposeIterator(it);
}
int main() {
    LinkedList list = nil();
    Cons(8, list);
    Cons(20, list);
    Cons(10, list);
    cout << "La sumatoria de 8, 20 y 10: " << sumatoria(list) << endl;
    Snoc(10, list);

}