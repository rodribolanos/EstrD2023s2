#include "Equipo.h"

struct EquipoSt {
    Seleccion seleccion;
    string nombre;
    string grupo;
};

Equipo crearEquipo(Seleccion s, string grupo) {
    Equipo e     = new EquipoSt;
    e->seleccion = s;
    e->nombre    = nombreDeSeleccion(s);
    e->grupo     = grupo;
    return e;
}

string nombreDeSeleccion(Seleccion s) {
    string selecciones[16];
    selecciones[ESTADOSUNIDOS] = "Estados Unidos";
    selecciones[COSTARICA]     = "Costa Rica";
    selecciones[COLOMBIA]      = "Colombia";
    selecciones[PARAGUAY]      = "Paraguay";
    selecciones[HAITI]         = "Haiti";
    selecciones[PERU]          = "Peru";
    selecciones[BRASIL]        = "Brasil";
    selecciones[ECUADOR]       = "Ecuador";
    selecciones[VENEZUELA]     = "Venezuela";
    selecciones[MEXICO]        = "Mexico";
    selecciones[URUGUAY]       = "Uruguay";
    selecciones[JAMAICA]       = "Jamaica";
    selecciones[ARGENTINA]     = "Argentina";
    selecciones[BOLIVIA]       = "Bolivia";
    selecciones[CHILE]         = "Chile";
    selecciones[PANAMA]        = "Panama";
    return selecciones[s];
}

Seleccion seleccionEquipo(Equipo e) {
    return e->seleccion;
}

string nombreEquipo(Equipo e) {
    return e->nombre;
}

string grupoEquipo(Equipo e) {
    return e->grupo;
}
