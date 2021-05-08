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

	satisfaceFila(RowN, PistasFilas, NewGrilla, FilaSat),
	% transpose/2 para rotar la grilla y asi las columnas se vuelven arreglos horizontales
	transpose(NewGrilla, GrillaRotada),
	satisfaceFila(ColN, PistasColumnas, GrillaRotada, ColSat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dada una lista de strings encuentra el string que es la concatenacion de todos los elementos de la lista
% reemplazando _ y X por un espacio en blanco
srtListConcat([], "").
srtListConcat([H | T], String) :-
	srtListConcat(T, Rest),
	% Si H es un _ o "X" reemplazar por " "
	((var(H) ; H = "X"),string_concat(" ", Rest, String), ! % <- este cut es importante para prevenir un backtrack
    														% si no se satisface una fila y prolog intenta hacer que lo haga
		;
	% En este caso H es "#"
	string_concat(H, Rest, String)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* extraerGrupos -> L:string, Grupos:[string]
Dada una fila "# ### " encuentra Grupos=["#", "###"] 
Split por " ", filtrar los elementos que no sean "" */
extraerGrupos(L, Grupos) :-
	split_string(L, " ", "", G),
	findall(X, (member(X, G), X \= ""), Grupos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
% Dado el numero de fila se la busca entre todas las PistasFilas.
obtenerPistasFila(0,[H|T], H).
obtenerPistasFila(N, [H|T], PF):- 
	N1 is N-1, 
	obtenerGrupoPistas(N1, PF).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Controla que las pistas de una fila y los grupos resultantes de las jugadas del usuario sean igual
%por ejemplo, para [1,4] el grupos ["#"","##"] es incorrecto pero ["#"","####"] si es correcto.
%verificarNumeros(PistasFila, Grupos)
verificarNumeros([P|Ps], [G|Gs]):-
	string_length(G, Length),
	P = Length, /* !,*/ % este cut era por un trace, pero no se necesita
	verificarNumeros(Ps,Gs).

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
	(	nth0(RowN, GrillaFilas, Row),    % obtener la RowN-esima fila
		srtListConcat(Row, RowString),   % convertirla en un string
		extraerGrupos(RowString, Grupos),

		% obtenerPistasFila(RowN, PistasFilas, PFila),
		nth0(RowN, PistasFilas, PFila),
		length(Grupos, CantGrupos), % la longitud de grupos		
		length(PFila, CantGrupos),  % y pistas debe ser igual
		% FilaSat = 1 si length(Grupos) == length(PistasFilas) y coinciden longitud de grupos con cada pista
		verificarNumeros(PFila, Grupos), % Esto verifica esa ultima propiedad
		FilaSat = 1
	), !;FilaSat = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% satisfaceColumna(ColN, PistasColumnas, GrillaColumnas, ColSat).