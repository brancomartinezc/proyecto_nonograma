:- module(proylcc,
	[  
		put/8
	]).
:-use_module(library(lists)).
:-use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY es el resultado de reemplazar la ocurrencia de X en la posición XIndex de Xs por Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Contenido, +Pos, +PistasFilas, +PistasColumnas, +Grilla, -GrillaRes, -FilaSat, -ColSat).
%

put(Contenido, [RowN, ColN], PistasFilas, PistasColumnas, Grilla, NewGrilla, FilaSat, ColSat):-
	% NewGrilla es el resultado de reemplazar la fila Row en la posición RowN de Grilla
	% (RowN-ésima fila de Grilla), por una fila nueva NewRow.
	replace(Row, RowN, NewRow, Grilla, NewGrilla),

	% NewRow es el resultado de reemplazar la celda Cell en la posición ColN de Row por _,
	% siempre y cuando Cell coincida con Contenido (Cell se instancia en la llamada al replace/5).
	% En caso contrario (;)
	% NewRow es el resultado de reemplazar lo que se que haya (_Cell) en la posición ColN de Row por Contenido.
	(replace(Cell, ColN, _, Row, NewRow), Cell == Contenido ; replace(_Cell, ColN, Contenido, Row, NewRow)),

	satisface(RowN, PistasFilas, NewGrilla, FilaSat),
	% transpose/2 para rotar la grilla y asi las columnas se vuelven arreglos horizontales
	transpose(NewGrilla, GrillaRotada),
	satisface(ColN, PistasColumnas, GrillaRotada, ColSat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% prepararLista(+Lista, -ListaPreparada).
%
% Prepara la lista para despues ingresarla en el split
% Ejemplo: [#,_,X,#,#,] --> [#,x,x,#,#]

prepararLista([],[]).
prepararLista([H|T],[x|R]):-
	(var(H) ; H == "X"),
    prepararLista(T,R), !.
prepararLista([H|T],[H|R]):-
    prepararLista(T,R), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% split(+Lista, +Separador, -ListaGrupos).
%
% Se extraen de la lista los grupos de #
% Ejemplo: [#,x,x,#,#] --> [[#],[#,#]]
% Comentario extra: la lista auxiliar mantiene los elems del grupo que se recorre en determinado momento. 
% Por eso cuando el primer elem es un pivot se va mandando una nueva lista auxiliar vacia, esto permite la separacion en sublistas de los grupos.

split(L,P,R):- split(L,P,[],R), !.
split([],_,[],[]). %CB1: si la lista es vacia y la lista auxiliar tambien, devuelve la lista vacia.
split([],_,Aux,[Aux]) :- Aux \= []. %CB2: si la lista es vacia y la lista auxiliar no lo es, devuelve la auxiliar como sublista de una nueva lista.
split([P|T],P,[],R) :- split(T,P,[],R). %CR1: si el primer elem de la lista es el pivot y la lista auxiliar es vacia,
										%sigue haciendo split con el resto y una nueva lista auxiliar vacia.
split([P|T],P,Aux,[Aux|R]) :- Aux \= [], split(T,P,[],R). %CR2: si el primer elem de la lista es el pivot y la lista auxiliar NO es vacia,
													%sigue haciendo split con el resto y nueva lista auxiliar vacia. 
													%Al volver de la recursion agrega la auxiliar a la lista resultado.
split([H|T],P,Aux,R) :- H \= P, append(Aux, [H], Aux2), split(T,P,Aux2,R). %CR3: si el primer elem de la lista no es el pivot, concatena la lista auxiliar  
																	%con el elem y sigue haciendo split con el resto de la lista y la nueva lista auxiliar.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% verificarSolucion(+Pistas, +Grupos).
%
% Controla que las pistas de una fila y los grupos resultantes de las jugadas del usuario sean igual
% Por ejemplo, para [1,4] el grupos ["#" ,"##"] es incorrecto pero ["#" ,"####"] si es correcto.
verificarSolucion([],[]).
verificarSolucion([P|Ps], [G|Gs]):-
	length(G,P),
	verificarSolucion(Ps,Gs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% satisface(+RowN, +PistasLineas, +GrillaLineas, -LineaSat).
%
% Controla si una linea (fila o columna) satisface las pistas.
% La forma del cuerpo con un grupo entre parentesis indica: si todo dentro se satisface,
% LineaSat se unifica con 1, pero si algo no lo hace, se toma el camino alternativo de
% unificar LineaSat con 0. Esto logra que el predicado nunca devuelva false, dejando
% que la parte de React tome decisiones respecto a LineaSat
satisface(RowN, PistasLineas, GrillaLineas, LineaSat) :- 
	(	nth0(RowN, GrillaLineas, Row),    % obtener la RowN-esima lineas de la grilla
		nth0(RowN, PistasLineas, PLinea), % obtener la RowN-esima lineas de pistas
		prepararLista(Row,LineaPreparada),
		split(LineaPreparada,x,Grupos),
		verificarSolucion(PLinea, Grupos),
		LineaSat = 1
	), !;LineaSat = 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Codigo de resolucion: Proyecto 2

/* Testear con
    solve([[3], [1,2], [3], [5], [5]],
      [[2], [5], [1,3], [5], [1, 2]],
      S), !.
      
      El cut al final es porque genera varias veces la misma solucion
      Entonces corta el backtracking y deja una sola hecha
      
      El resultado S es el arreglo que tiene N arreglos, cada uno con todas las soluciones
      de las N filas del tablero.
*/

% PF es PistasFilas, PC es PistasColumnas
% Intercambiar TodasLasSoluciones por GrillaResuelta despues
solve(PF, PC, TodasLasSoluciones) :-
    length(PC, CantidadPistas),
    length(PF, NFilas),
    generarSolucionesDeTodasLasFilas(CantidadPistas, PF, Soluciones),
    % Para hacer despues: invertir la generacion de soluciones en el predicado anterior elimina este reverse
    reverse(Soluciones, TodasLasSoluciones),
    !, % no mas backtracking desde este punto, ya tenemos las soluciones

    % Con TodasLasSoluciones seleccionar de a un elemento de cada uno
    % construir la grilla, transponer y checkear que se satisfacen todas las columnas
    % Como las filas ya estan trivialmente satisfechas, cuando todas las columnas se satisfacen 
    % a la vez, esta resuelto el tablero, y se puede devolver a la parte de JS
    % Multiples soluciones para un tablero?
    testearCombinaciones(NFilas, PC, TodasLasSoluciones).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% agregar un parametro mas que tiene la grilla resuelta
testearCombinaciones(NFilas, _PistasColumnas, Soluciones) :-
    configuracion(0, NFilas, Soluciones, Config),
    write(Config), nl. % debug
    % testearSolucion deberia transponer la config. y testearlo con los predicados que ya tenemos
    % testearSolucion(...)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% configuracion(+Indice, +CantidadDeFilas, +SolucionesDeTodasLasFilas, +ConfiguracionPosible)
% Toma de cada grupo de soluciones por fila una sola, y arma la grilla en +ConfiguracionPosible
configuracion(NFilas, NFilas, _Soluciones, []).
configuracion(Idx, NFilas, Soluciones, Configuracion) :-
    Siguiente is Idx + 1,
    nth0(Idx, Soluciones, FilaPosibleSoluciones),
    member(SolucionPosible, FilaPosibleSoluciones),
    configuracion(Siguiente, NFilas, Soluciones, RestoDeLaConfig),
    append([SolucionPosible], RestoDeLaConfig, Configuracion).

% ~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+
% ~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+
% generarSolucionesDeTodasLasFilas(+CantidadFilas, +PistasFilas, -ListaSoluciones)
generarSolucionesDeTodasLasFilas(0, _PF, []) :- !.

% CantidadFilas es el indice
generarSolucionesDeTodasLasFilas(CantidadFilas, PistasFilas, ListaSoluciones) :-
    nth1(CantidadFilas, PistasFilas, PistaFilaActual), % seleccionar la ultima pista
    CantPrev is CantidadFilas - 1,
    length(PistasFilas, LongitudFila),
    generarSolucionesDeUnaFila(PistaFilaActual, LongitudFila, Soluciones), % encontrar las soluciones para la ultima fila segun su pista
    generarSolucionesDeTodasLasFilas(CantPrev, PistasFilas, RestoDeLasSoluciones), % recursivamente continuar
    append([Soluciones], RestoDeLasSoluciones, ListaSoluciones).

% ~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+
% [1,3] -> [todas las soluciones de longitud LongitudLinea que conforman [1,3]]
generarSolucionesDeUnaFila(PistasFila, LongitudLinea, SolucionesFila) :-
    length(Solucion, LongitudLinea),
    grupos(PistasFila, Grupos),
    findall(Solucion, validarEspaciado(Grupos, Solucion), SolucionesFila).

% % ~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+
% % ~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+
% % ~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+
% Para testear lo que esta abajo de esta linea llamar con algo como
% length(_L, 10), grupos([1,1,1], _G), findall(_L, validarEspaciado(_G, _L), Ls).

generarGrupo(0, []).
generarGrupo(P, L) :-
    Prev is P - 1,
    generarGrupo(Prev, SubL),
    append(["#"], SubL, L),
    !. % no seguir generando

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Caso donde solo hay una pista
grupos([P], [Grupo]) :-
    generarGrupo(P, Grupo).

% Caso donde hay multiples pistas
% Generar la primera, y despues generar el resto con espacios adelante
grupos([P | Ps], [Grupo | Grupos]) :-
    generarGrupo(P, Grupo),
    gruposConEspacio(Ps, Grupos).

% despues del primero, generar el resto de los grupos con un espacio adelante
% Para descartar los casos donde van juntos
gruposConEspacio([], []) :- !.
gruposConEspacio([P | Ps], [GrupoV1 | Grupos]) :-
    generarGrupo(P, Grupo),
    append(["X"], Grupo, GrupoV1),
    gruposConEspacio(Ps, Grupos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% validarEspaciado(+Grupos, +Linea)

% Caso donde no hay grupos
validarEspaciado([], []) :- !.

% Caso donde la linea empieza con "X"
% Consume todas las "X"s iniciales
validarEspaciado(Grupos, ["X" | Resto]) :-
    validarEspaciado(Grupos, Resto).

% Caso donde no comienza con "X" (porque si lo hace entraria a la otra regla)
validarEspaciado([G|Gs], Linea) :-
    comienzaCon(G, Linea, Resto), % comienzaCon consume el grupo G del principio de la linea, y deja lo sobrante en Resto
    validarEspaciado(Gs, Resto). % seguir validando los otros grupos con el sobrante de la linea

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comienzaCon(+Grupo, +Linea, +RestoDeLaLinea)

% Si el grupo esta vacio no se consume nada y el resto es la linea misma
comienzaCon([], Linea, Linea).

% Si el grupo no esta vacio, se espera que el primer elemento del grupo sea
% el mismo que el de la linea (por eso la misma variable P).
% Se llama recursivamente a comienzaCon hasta encontrar todo el grupo en la linea
% Resto queda con el resto de la linea para seguir buscando grupos en validarEspaciado
comienzaCon([P|Ps], [P | Linea], Resto) :-
    comienzaCon(Ps, Linea, R),
    append(R,[], Resto).