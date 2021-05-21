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
