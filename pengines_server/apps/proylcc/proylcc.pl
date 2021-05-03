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

put(Contenido, [RowN, ColN], _PistasFilas, _PistasColumnas, Grilla, NewGrilla, FilaSat, ColSat):-
	% NewGrilla es el resultado de reemplazar la fila Row en la posición RowN de Grilla
	% (RowN-ésima fila de Grilla), por una fila nueva NewRow.
	replace(Row, RowN, NewRow, Grilla, NewGrilla),

	% NewRow es el resultado de reemplazar la celda Cell en la posición ColN de Row por _,
	% siempre y cuando Cell coincida con Contenido (Cell se instancia en la llamada al replace/5).
	% En caso contrario (;)
	% NewRow es el resultado de reemplazar lo que se que haya (_Cell) en la posición ColN de Row por Contenido.
	(replace(Cell, ColN, _, Row, NewRow), Cell == Contenido ; replace(_Cell, ColN, Contenido, Row, NewRow)),

	satisfaceFila(RowN, PistasFilas, NewGrilla, FilaSat),
	% transpose/2 para rotar la grilla y asi las columnas se vuelven arreglos horizontales
	transpose(NewGrilla, GrillaRotada),
	satisfaceColumna(ColN, PistasColumnas, NewGrillaRotada, ColSat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dada una lista de strings encuentra el string que es la concatenacion de todos los elementos de la lista
% reemplazando _ y X por un espacio en blanco
srtListConcat([], "").
srtListConcat([H | T], String) :-
	srtListConcat(T, Rest),
	% Si H es un _ o "X" reemplazar por " "
	% implementar con un findall?
	((var(H) ; H = "X"),string_concat(" ", Rest, String)
		;
	string_concat(H, Rest, String)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* extraerGrupos -> L:string, Grupos:[string]
Dada una fila "# ### " encuentra Grupos=["#", "###"] 
Split por " ", filtrar los elementos que no sean "" */
extraerGrupos(L, Grupos) :-
	split_string(L, " ", "", G),
	findall(X, (member(X, G), X \= ""), Grupos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* La fila va a ser invalida hasta que este completa
	Es decir, "#X## " no satisface las pistas [1, 3]
	Verificar desde el lado de Javascript cuando marcar como incorrecto?
	Cuando determinar si marcar algo como incorrecto segun los valores de FilaSat y ColSat?

	Probe varias versiones en internet y cada una decide de forma distinta
	Algunas te marcan incorrecto si una fila o columna tiene mas
		grupos que cantidad de pistas (e.g. "# # #" con pistas [1, 1])

	Otros no te marcan hasta que se marca un recuadro incorrecto (es decir, tienen el tablero
		resuelto y te indican si pintaste un cuadro que no esta pintado en la grilla resuelta)
*/
satisfaceFila(RowN, PistasFilas, GrillaFilas, FilaSat) :- 
	nth1(RowN, GrillaFilas, Row),    % obtener la RowN-esima fila
	srtListConcat(Row, RowString),   % convertirla en un string
	extraerGrupos(RowString, Grupos),
	length(Grupos, CantGrupos),      % la longitud de grupos
	length(PistasFilas, CantGrupos). % y pistas debe ser igual
	% FilaSat = 1 si length(Grupos) == length(PistasFilas) y coinciden longitud de grupos con cada pista?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
satisfaceColumna(ColN, PistasColumnas, GrillaColumnas, ColSat).