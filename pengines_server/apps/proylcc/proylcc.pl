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
    solve([[3], [1,2], [3], [5], [5]], [[2], [5], [1,3], [5], [1, 2]], S), !
    solve([[2,1], [1,3], [1,2], [3], [4]], [[1], [5], [2], [4], [2,1], [2]], S), !

    El cut al final es porque genera varias veces la misma solucion
    Entonces corta el backtracking y deja una sola hecha

    Generar una configuracion correcta de entre estas soluciones
    configuracion( [[["X", "X", "#", "#", "#"], ["X", "#", "#", "#", "X"], ["#", "#", "#", "X", "X"]], [["X", "#", "X", "#", "#"], ["#", "X", "X", "#", "#"], ["#", "X", "#", "#", "X"]], [["X", "X", "#", "#", "#"], ["X", "#", "#", "#", "X"], ["#", "#", "#", "X", "X"]], [["#", "#", "#", "#", "#"]], [["#", "#", "#", "#", "#"]]], Config), !
*/

% solve toma las pistas de las filas y de las columnas
% Encuentra en GrillaResuelta una solucion para el tablero (si existe)
% Las dimensiones se infieren de la longitud de las listas de pistas
solve(PistasFilas, PistasColumnas, GrillaResuelta) :-
    length(PistasColumnas, LongitudFila),
    generarSolucionesDeTodasLasFilas(PistasFilas, LongitudFila, TodasLasSoluciones),
    !, % no mas backtracking desde este punto, ya tenemos las soluciones de cada fila

    testearCombinaciones(PistasColumnas, TodasLasSoluciones, GrillaResuelta).
	% write("Resultado "), write(GrillaResuelta), nl.

%%%%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%%%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% testearCombinaciones(+PistasColumnas, +Soluciones, -Config)
% Dado un tablero NxM testearCombinaciones(N, [M pistas], [N arreglos de soluciones de cada una de las N filas], GrillaFinal)
% encuentra una config que satisface las columnas al transponer la grilla compuesta por la eleccion de
% una solucion por fila de entre el arreglo de soluciones
% 
testearCombinaciones(PistasColumnas, Soluciones, Config) :-
    configuracion(Soluciones, Config), % config es una grilla posible tomando de a una solucion por fila

    % Se transpone esta grilla para ver si las columnas son satisfechas
    % Como las filas estan compuestas siempre de sus soluciones, con solo validar las columnas es suficiente
    % para determinar si tenemos una solucion valida al tablero
    transpose(Config,ConfigRotada),
    testearSolucion(ConfigRotada,PistasColumnas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% testearSolucion(+Lineas, +Pistas)
% Dado un tablero transpuesto donde se asume que las columnas (antes filas) ya tienen una solucion correcta
% se verifica que todas las filas (antes columnas) satisfacen sus pistas correspondientes
testearSolucion([],[]).
testearSolucion([L|Ls],[P|Ps]):-
    split(L,"X",Grupos),
    verificarSolucion(P,Grupos),
    testearSolucion(Ls,Ps). 

%%%%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%%%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% configuracion(+SolucionesDeTodasLasFilas, -ConfiguracionPosible)
% Genera una configuracion de tablero a partir de todas las soluciones
% posibles para cada fila. Se pueden obtener todas con un findall o similar
configuracion([], []).
configuracion([Soluciones | RestoDeLasSoluciones], Configuracion) :-
    member(SolucionPosible, Soluciones), % tomar uno de entre todas las soluciones de una de las filas
    configuracion(RestoDeLasSoluciones, RestoDeLaConfig), % hacer lo mismo con el resto de las filas
    append([SolucionPosible], RestoDeLaConfig, Configuracion). % ubicarlo todo en una grilla

% ~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+
% ~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+

% generarSolucionesDeTodasLasFilas(+Pistas, +LongitudFila, -ListaSoluciones)
% Dadas las pistas y la longitud de las filas, se generan todas las soluciones
% de cada fila posibles, una a una (usando generarSolucionesDeUnaFila)
generarSolucionesDeTodasLasFilas([], _LF, []).
generarSolucionesDeTodasLasFilas([P | Ps], LongitudFila, ListaSoluciones) :-
    generarSolucionesDeUnaFila(P, LongitudFila, Soluciones),
    generarSolucionesDeTodasLasFilas(Ps, LongitudFila, RestoDeLasSoluciones),
    append([Soluciones], RestoDeLasSoluciones, ListaSoluciones).

% ~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~+
% [1,3] -> [todas las soluciones de longitud LongitudLinea que conforman [1,3]]
% Se generan con un findall que encuentra solo las lineas de LongitudLinea correctas
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