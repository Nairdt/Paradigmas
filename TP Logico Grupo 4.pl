% - - - Parte 1 - - -

%pareja(Persona, Persona)
pareja(marsellus, mia).
pareja(pumkin,    honeyBunny).
pareja(bernardo, bianca).
pareja(bernardo, charo).

%trabajaPara(Empleador, Empleado)
trabajaPara(marsellus, vincent).
trabajaPara(marsellus, jules).
trabajaPara(marsellus, winston).
trabajaPara(Empleador, bernardo):-
	trabajaPara(marsellus, Empleador),
	Empleador \= jules.
trabajaPara(Empleador, george):-
	saleCon(bernardo, Empleador).

saleCon(Quien, Cual):-
	pareja(Quien, Cual).
saleCon(Quien, Cual):-
	pareja(Cual, Quien).

esFiel(Persona):- 
	saleCon(Persona, _),
	forall(saleConVarios(Persona, UnaPersona, OtraPersona), not(UnaPersona \= OtraPersona)).
	
saleConVarios(A, B, C):-
	saleCon(A, B),
	saleCon(A, C).

acataOrden(DaOrden, RecibeOrden):-
	trabajaPara(DaOrden, RecibeOrden).
acataOrden(DaOrden, RecibeOrden):-
	trabajaPara(DaOrden, Intermediario),
	acataOrden(Intermediario, RecibeOrden).
	
% - - - Parte 2 - - - 

% Información base
% personaje(Nombre, Ocupacion)
personaje(pumkin,     ladron([estacionesDeServicio, licorerias])).
personaje(honeyBunny, ladron([licorerias, estacionesDeServicio])).
personaje(vincent,    mafioso(maton)).
personaje(jules,      mafioso(maton)).
personaje(marsellus,  mafioso(capo)).
personaje(winston,    mafioso(resuelveProblemas)).
personaje(mia,        actriz([foxForceFive])).
personaje(butch,      boxeador).
personaje(bernardo,   mafioso(cerebro)).
personaje(bianca,     actriz([elPadrino1])).
personaje(elVendedor, vender([humo, iphone])).
personaje(jimmie,     vender([auto])).

% encargo(Solicitante, Encargado, Tarea). 
% las tareas pueden ser cuidar(Protegido), ayudar(Ayudado), buscar(Buscado, Lugar)
encargo(marsellus, vincent,   cuidar(mia)).
encargo(vincent,  elVendedor, cuidar(mia)).
encargo(marsellus, winston, ayudar(jules)).
encargo(marsellus, winston, ayudar(vincent)).
encargo(marsellus, vincent, buscar(butch, losAngeles)).
encargo(bernardo, vincent, buscar(jules, fuerteApache)).
encargo(bernardo, winston, buscar(jules, sanMartin)).
encargo(bernardo, winston, buscar(jules, lugano)).

amigo(vincent, jules).
amigo(jules, jimmie).
amigo(vincent, elVendedor).

esPeligroso(Personaje):-
	personaje(Personaje, Ocupacion),
	actividadPeligrosa(Ocupacion).
esPeligroso(Personaje):-
	trabajaPara(Jefe, Personaje),
	esPeligroso(Jefe).

actividadPeligrosa(mafioso(maton)).
actividadPeligrosa(ladron(RobaEn)):- member(licorerias, RobaEn).

sanCayetano(Personaje):-
	tieneCerca(Personaje, _),
	forall(tieneCerca(Personaje, Cercano), todosTienenEncargo(Personaje, Cercano)).
	
tieneCerca(Personaje, Cercano):- sonAmigos(Personaje, Cercano).
tieneCerca(Personaje, Cercano):- hayRelacionLaboral(Personaje, Cercano).

sonAmigos(Uno, Otro):- amigo(Uno, Otro).
sonAmigos(Uno, Otro):- amigo(Otro, Uno).

hayRelacionLaboral(Uno, Otro):- trabajaPara(Uno, Otro).
hayRelacionLaboral(Uno, Otro):- trabajaPara(Otro, Uno).

todosTienenEncargo(Personaje, Cercano):- encargo(Personaje, Cercano, _).

nivelDeRespeto(Personaje, Nivel):-
	personaje(Personaje, actriz(Lista)),
	length(Lista, Cant),
	Nivel is Cant / 10.
nivelDeRespeto(Personaje, 10):- personaje(Personaje, mafioso(resuelveProblemas)).
nivelDeRespeto(Personaje, 20):- personaje(Personaje, mafioso(capo)).
nivelDeRespeto(vincent, 15).

respetabilidad(Respetables, NoRespetables):-
	findall(SonRespetables, (nivelDeRespetoCompleto(SonRespetables, NivelResp), mayorA9(NivelResp)), ListaResp),
	findall(SonNoRespetables, (nivelDeRespetoCompleto(SonNoRespetables, NivelNoResp), not(mayorA9(NivelNoResp))), ListaNoResp),
	length(ListaResp, Respetables),
	length(ListaNoResp, NoRespetables).
	
nivelDeRespetoCompleto(Personaje, Nivel):- nivelDeRespeto(Personaje, Nivel).
nivelDeRespetoCompleto(Personaje, 0):- 
	personaje(Personaje, _),
	not(nivelDeRespeto(Personaje, _)).
	
mayorA9(Nivel):- Nivel > 9.

cantidadEncargos(Personaje, CantEncargos):-
	encargo(_, Personaje, _),
	findall(Encargos, encargo(_, Personaje, Encargos), ListaEncargos),
	length(ListaEncargos, CantEncargos).
	
masAtareado(Quien):-
	encargo(_, Quien, _),
	findall(NumeroEncargos, cantidadEncargos(_, NumeroEncargos), ListaCantEncargos),
	forall(cantidadEncargos(Quien, CantEncargos), max_member(CantEncargos, ListaCantEncargos)).
	
	