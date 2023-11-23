#include <iostream>
#include <vector>
#include "Equipo.h"
#include "Test.h"

using namespace std;

int main()
{
    cout << "Inicializando equipos..." << endl;

    Equipo estadosUnidos = crearEquipo(ESTADOSUNIDOS, "A");
    Equipo costaRica     = crearEquipo(COSTARICA,     "A");
    Equipo colombia      = crearEquipo(COLOMBIA,      "A");
    Equipo paraguay      = crearEquipo(PARAGUAY,      "A");

    Equipo haiti         = crearEquipo(HAITI,         "B");
    Equipo peru          = crearEquipo(PERU,          "B");
    Equipo brasil        = crearEquipo(BRASIL,        "B");
    Equipo ecuador       = crearEquipo(ECUADOR,       "B");

    Equipo venezuela     = crearEquipo(VENEZUELA,     "C");
    Equipo mexico        = crearEquipo(MEXICO,        "C");
    Equipo uruguay       = crearEquipo(URUGUAY,       "C");
    Equipo jamaica       = crearEquipo(JAMAICA,       "C");

    Equipo argentina     = crearEquipo(ARGENTINA,     "D");
    Equipo bolivia       = crearEquipo(BOLIVIA,       "D");
    Equipo chile         = crearEquipo(CHILE,         "D");
    Equipo panama        = crearEquipo(PANAMA,        "D");

    cout << "Probando UFSet sobre equipos de la Copa..." << endl << endl;

    // Test 1
    vector<Equipo> caso1;
    caso1.push_back(argentina);
    caso1.push_back(chile);
    caso1.push_back(venezuela);
    caso1.push_back(colombia);
    caso1.push_back(paraguay);
    caso1.push_back(mexico);
    caso1.push_back(uruguay);
    test("Caso 1", caso1);

    // Test 2
    vector<Equipo> caso2;
    caso2.push_back(estadosUnidos);
    caso2.push_back(brasil);
    caso2.push_back(jamaica);
    caso2.push_back(argentina);
    caso2.push_back(uruguay);
    test("Caso 2", caso2);

    // Test 3
    vector<Equipo> caso3;
    caso3.push_back(estadosUnidos);
    caso3.push_back(costaRica);
    caso3.push_back(colombia);
    caso3.push_back(paraguay);

    caso3.push_back(haiti);
    caso3.push_back(peru);
    caso3.push_back(brasil);
    caso3.push_back(ecuador);

    caso3.push_back(venezuela);
    caso3.push_back(mexico);
    caso3.push_back(uruguay);
    caso3.push_back(jamaica);

    caso3.push_back(argentina);
    caso3.push_back(bolivia);
    caso3.push_back(chile);
    caso3.push_back(panama);
    test("Caso 3", caso3);

    UFSet uFestadosUnidos = createUFS(estadosUnidos);
    UFSet uFcostaRica     = createUFS(costaRica);
    UFSet uFcolombia      = createUFS(colombia);
    UFSet uFparaguay      = createUFS(paraguay);
    UFSet uFhaiti         = createUFS(haiti);
    UFSet uFmexico        = createUFS(mexico);
    UFSet uFargentina     = createUFS(argentina);
    UFSet uFbolivia       = createUFS(bolivia);
    
    // Casos de test
    unionUFS(uFestadosUnidos, uFcostaRica); unionUFS(uFcolombia, uFparaguay);
    cout << "El representante de Costa Rica: " << nombreEquipo(elemUFS(findUFS(uFcostaRica))) << endl;
    cout << "El grupo de Costa Rica: " << grupoEquipo(elemUFS(findUFS(uFcostaRica))) << endl;
    cout << "El grupo de Estados Unidos: " << grupoEquipo(elemUFS(findUFS(uFestadosUnidos))) << endl;
    cout << "El representante de Colombia previo a la union con estados Unidos: " << nombreEquipo(elemUFS(findUFS(uFcolombia))) << endl;
    unionUFS(uFestadosUnidos, uFcolombia);
    cout << "El representante de Colombia post a la union con estados Unidos: " << nombreEquipo(elemUFS(findUFS(uFcolombia))) << endl;
    // En este punto, estados unidos es de grado 2.
    // Creamos dos uF de rango 1.
    unionUFS(uFhaiti, uFmexico); unionUFS(uFargentina, uFbolivia); 
    // Unimos dos uF de rango 1, para que nos quede haiti como padre.
    unionUFS(uFhaiti, uFargentina);
    cout << "En este punto, el padre de argentina es haiti ya que tenian el mismo rango. findUFS(argentina): " << nombreEquipo(elemUFS(findUFS(uFargentina))) << endl;
    // Un union de dos UFSet de mismo rango, hace que el padre del segundo sea el padre del primero.
    unionUFS(uFestadosUnidos, uFargentina);
    cout << "El padre de argentina tiene que ser estados unidos. Padre de argentina: " << nombreEquipo(elemUFS(findUFS(uFargentina))) << endl;
    cout << "El padre de haiti tiene que ser estados unidos, ya que era el padre de argentina. Padre de haiti: " << nombreEquipo(elemUFS(findUFS(uFhaiti))) << endl;
   
    cout << "La direccion de memoria del padre de Estados Unidos: " << findUFS(uFestadosUnidos) << endl;
    cout << "La direccion de memoria del padre de Argentia: " << findUFS(uFargentina) << endl;
    unionUFS(uFestadosUnidos, uFargentina);
    cout << "Realizar una union entre dos ufSet con el mismo padre, no modifica nada." << endl;
    cout << "La direccion de memoria del padre de Estados Unidos: " << findUFS(uFestadosUnidos) << endl;
    cout << "La direccion de memoria del padre de Argentia: " << findUFS(uFargentina) << endl;
    cout << "La direccion de memoria del padre de Estados Unidos: " << findUFS(uFestadosUnidos) << endl;    
     return 0;
}
