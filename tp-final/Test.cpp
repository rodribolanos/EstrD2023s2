#include "Test.h"

void imprimirEquipos(vector<Equipo> equipos) {
    cout << "Equipos { " << endl;
    for (vector<Equipo>::iterator it = equipos.begin() ; it != equipos.end(); ++it) {
        cout << "  " << nombreEquipo(*it) << endl;
    }
    cout << "}" << endl << endl;
}

void imprimirSingletons(vector<UFSet> singletons) {
    cout << "Singletons { " << endl;
    for (vector<UFSet>::iterator it = singletons.begin() ; it != singletons.end(); ++it) {
        cout << "  { " << nombreEquipo(elemUFS(*it)) << " }" << endl;
    }
    cout << "}" << endl << endl;
}

void imprimirCombinaciones(vector< pair<Equipo, Equipo> > equipos) {
    cout << "Pares de Equipos (combinaciones) { " << endl;
    for (vector< pair<Equipo, Equipo> >::iterator it = equipos.begin() ; it != equipos.end(); ++it) {
        cout << "  (" << nombreEquipo((*it).first) << "," << nombreEquipo((*it).second) << ")" << endl;
    }
    cout << "}" << endl << endl;
}

vector< pair<Equipo, Equipo> > combinacionesPorGrupos(vector<Equipo> equipos) {
    vector< pair<Equipo, Equipo> > result;
    for (vector<Equipo>::iterator it1 = equipos.begin() ; it1 != equipos.end(); ++it1) {
        for (vector<Equipo>::iterator it2 = equipos.begin() ; it2 != equipos.end(); ++it2) {
            if(grupoEquipo(*it1) == grupoEquipo(*it2)) {
                result.push_back(make_pair(*it1, *it2));
            }
        }
    }
    return result;
}

vector<UFSet> inicializar(vector<Equipo> equipos) {
    vector< UFSet > result;
    for (vector<Equipo>::iterator it = equipos.begin() ; it != equipos.end(); ++it) {
        result.push_back(createUFS(*it));
    }
    return result;
}

void combinarSingletons(vector<UFSet> singletons) {
    for (vector<UFSet>::iterator it1 = singletons.begin() ; it1 != singletons.end(); ++it1) {
        for (vector<UFSet>::iterator it2 = singletons.begin() ; it2 != singletons.end(); ++it2) {
            UFSet ufset1 = *it1;
            UFSet ufset2 = *it2;
            if(grupoEquipo(elemUFS(ufset1)) == grupoEquipo(elemUFS(ufset2))) {
                unionUFS(ufset1, ufset2);
                break;
            }
        }
    }
}

int cantidadParticiones(vector<UFSet> singletons) {
    int result = 0;
    for (vector<UFSet>::iterator it = singletons.begin() ; it != singletons.end(); ++it) {
        if(findUFS(*it) == (*it)) {
            result++;
        }
    }
    return result;
}

void imprimirRepresentantes(vector<UFSet> singletons) {
    cout << "Equipo -> Representante { " << endl;
    for (vector<UFSet>::iterator it = singletons.begin(); it != singletons.end(); ++it) {
        UFSet ufset = *it;
        cout << "  " << nombreEquipo(elemUFS(ufset)) << " --> " << nombreEquipo(elemUFS(findUFS(ufset))) << endl;
    }
    cout << "}" << endl << endl;
}

void test(string titulo, vector<Equipo> equipos) {
    cout << titulo << ": " << endl << endl;
    cout << "--------------------------------------------------------------------------" << endl << endl;
    cout << "Cantidad de equipos: " << equipos.size() << endl;
    imprimirEquipos(equipos);

    cout << "--------------------------------- FASE 1 ---------------------------------" << endl;
    vector<UFSet> singletons = inicializar(equipos);
    cout << "Cantidad de singletons: " << singletons.size() << endl;
    imprimirSingletons(singletons);

    cout << "--------------------------------- FASE 2 ---------------------------------" << endl;
    vector< pair<Equipo, Equipo> > combinaciones = combinacionesPorGrupos(equipos);
    cout << "Cantidad de combinaciones a recorrer: " << combinaciones.size() << endl;
    imprimirCombinaciones(combinaciones);
    combinarSingletons(singletons);

    cout << "Cantidad de representantes: " << cantidadParticiones(singletons) << endl;
    imprimirRepresentantes(singletons);

    cout << "FIN ----------------------------------------------------------------------" << endl << endl;
}
