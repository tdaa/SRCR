%Sistemas de Representação de Conhecimento e Raciocínio
%Exercício 1 - Prestação de Cuidados de Saúde

:- set_prolog_flag( discontiguous_warnings, off).
:- set_prolog_flag( single_var_warnings, off).
:- set_prolog_flag( unknown, fail).

%Definições iniciais

:- op(900, xfy, '::').
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic instituicao/3.
:- dynamic cuidado/6.

%Extensão do predicado soluçes

solucoes(X,Y,Z) :- findall(X,Y,Z).

%Extensão do predicado comprimento

comprimento(S,N) :- length(S,N).

%Extensão do predicado teste

teste([]).
teste([R|L]) :- R, teste(L).

%Extensão do predicado remover

remove(P) :- retract(P).
remove(P) :- assert(P), !, fail.

%Extensão do predicado insere

insere(P) :- assert(P).
insere(P) :- retract(P), !, fail.

%Extensão do predicado evolucao

evolucao(Termo) :- solucoes(Inv, +Termo :: Inv, S),
					 insere(Termo), 
					 teste(S).

%Extensão do predicado inevolucao

inevolucao(Termo) :- solucoes(Inv, -Termo :: Inv, S),
					 remove(Termo),
					 teste(S).

%Extensão do predicado utente: #IdUt, Nome, Idade, Morada -> {V,F}

utente(1, tiago_alves, 20, vila_verde).
utente(2, isabel_pereira, 21, vila_verde).
utente(3, francisco_matos, 16, tenoes).
utente(4, maria_teixeira, 21, gondizalves).

%Extensão do predicado prestador: #IdPres, Nome, Especialidade, #IdInst -> {V,F}

prestador(1, tania_fernandes, psiquiatria, 3).
prestador(2, mario_oliveira, clinicaGeral, 2).
prestador(3, filipa_ferreira, pediatria, 1).
prestador(4, joao_machado, dermatologia, 2).
prestador(5, andre_correia, psiquiatria, 4).

%Extensão do predicado instituicao: #IdInst, Designacao, Localidade -> {V,F}

instituicao(1, hospital_de_braga, braga).
instituicao(2, hospital_da_trofa, braga).
instituicao(3, hospital_sao_joao, porto).
instituicao(4, hospital_da_luz, lisboa).

%Extensão do predicado cuidado: data, hora, #IdUt, #IdPrest, Descricao, Custo -> {V,F}

cuidado(2018-4-10, 16:00, 2, 1, ansiedade, 4).
cuidado(2018-5-16, 9:00, 2, 4, alergia_na_pele, 60).
cuidado(2018-6-2, 15:30, 3, 3, checkup, 10).
cuidado(2018-10-20, 10:45, 1, 4, acne, 60).
cuidado(2018-9-30, 11:00, 3, 5, stress, 30).

%Invariante Estrutural: não permitir a inserção de conhecimento repetido

+utente(ID,N,I,M)  :: (solucoes(ID, utente(ID,X,Y,Z), S),
					   comprimento(S,L),
					   L =< 1
					  ).

+prestador(ID,N,E,I)  :: (solucoes(ID, prestador(ID,X,Y,Z), S),
					      comprimento(S,L),
					      L =< 1
					     ).

+instituicao(ID,D,C)  :: (solucoes(ID, instituicao(ID,X,Y), S),
					      comprimento(S,L),
					      L =< 1
					     ).

%Invariante Referencial: não permitir a inserção de cuidados com a mesma data e hora para o mesmo utente ou para o mesmo prestador

+cuidado(D,H,IdU,IdP,DC,C)  :: (solucoes((D,H,IdU), cuidado(D,H,IdU,X,Y,Z), S),
					    		comprimento(S,L),
					    		L =< 1
					    	   ).

+cuidado(D,H,IdU,IdP,DC,C)  :: (solucoes((D,H,IdP), cuidado(D,H,X,IdP,Y,Z), S),
					    		comprimento(S,L),
					    		L =< 1
					    	   ).

%Invariante Referencial: não permitir a remoção de conhecimento que esteja a ser utilizado
-utente(ID,N,I,M) :: (solucoes( ID, cuidado(A,H,ID,IDp,D,C), S),
					  comprimento(S,L),
					  L==0
					 ).

-prestador(ID,N,E,IDi) :: (solucoes( ID, cuidado(A,H,IDu,ID,D,C), S),
						   comprimento(S,L),
						   L==0
					   	  ).

-instituicao(ID,N,M) :: (solucoes( ID, prestador(IDp,Np,E,ID), S),
						 comprimento(S,L),
						 L==0
					   	).

%Eliminar todas as ocorrências de um deteminado elemento de uma lista
apagaT([],_,[]).
apagaT([X|T],X,R) :- apagaT(T,X,R).
apagaT([X|T],Y,[X|R]) :- X \= Y, apagaT(T,Y,R).

%Eliminar elementos repetidos de uma lista
apagaRepetidos([],[]).
apagaRepetidos([H|T],[H|R]) :- apagaT(T,H,L), apagaRepetidos(L,R).

%Identificar utentes por critérios de seleção
identificaUtenteID(ID,N) :- utente(ID,N,I,M).
identificaUtenteNome(N,S) :- solucoes((ID,N), utente(ID,N,I,M), S).
identificaUtenteIdade(I,S) :- solucoes((ID,N), utente(ID,N,I,M), S).
identificaUtenteMorada(M,S) :- solucoes((ID,N), utente(ID,N,I,M), S).

%Identificar prestadores por critérios de seleção
identificaPrestadorID(ID,N) :- prestador(ID,N,E,IDi).
identificaPrestadorNome(N,S) :- solucoes((ID,N), prestador(ID,N,E,IDi), S).
identificaPrestadorEsp(E,S) :- solucoes((ID,N), prestador(ID,N,E,IDi), S).
identificaPrestadorIDinst(IDi,S) :- solucoes((ID,N), prestador(ID,N,E,IDi), S).
identificaPrestadorLocal(L,S) :- solucoes((ID,N), (prestador(ID,N,_,IDi), instituicao(IDi,_,L)), S).

%Identificar as instituições prestadoras de cuidados de saúde por critérios de seleção
identificaInstID(ID,N) :- instituicao(ID,N,L).
identificaInstNome(N,S) :- solucoes((ID,N,L), instituicao(ID,N,L), S).
identificaInstLoc(L,S) :- solucoes((ID,N), instituicao(ID,N,L), S).

%Identificar cuidados de saúde prestados por critérios de seleção tais como utente/prestador/instituição/cidade/datas
identificaCuidadoIDU(IDu,R) :- solucoes((D,H,IDu,IDp,DC,C), cuidado(D,H,IDu,IDp,DC,C), S), apagaRepetidos(S,R).
identificaCuidadoIDP(IDp,R) :- solucoes((D,H,IDu,IDp,DC,C), cuidado(D,H,IDu,IDp,DC,C), S), apagaRepetidos(S,R).
identificaCuidadoIDInst(IDi,R) :- solucoes((D,H,IDu,IDp,DC,C), (prestador(IDp,_,_,IDi), cuidado(D,H,IDu,IDp,DC,C)), S), apagaRepetidos(S,R).
identificaCuidadoNomeInst(Ni,R) :- solucoes((D,H,IDu,IDp,DC,C), (instituicao(IDi,Ni,_), prestador(IDp,_,_,IDi), cuidado(D,H,IDu,IDp,DC,C)), S), apagaRepetidos(S,R).
identificaCuidadoCidade(L,R) :- solucoes((D,H,IDu,IDp,DC,C), (instituicao(IDi,_,L), prestador(IDp,_,_,IDi), cuidado(D,H,IDu,IDp,DC,C)), S), apagaRepetidos(S,R).
identificaCuidadoData(D,R) :- solucoes((D,H,IDu,IDp,DC,C), cuidado(D,H,IDu,IDp,DC,C), S), apagaRepetidos(S,R).
identificaCuidadoDataHora(D,H,R) :- solucoes((D,H,IDu,IDp,DC,C), cuidado(D,H,IDu,IDp,DC,C), S).

%Identificar utentes de um prestador/especialidade/instituicao
identificaUtenteIDP(IDp,R) :- solucoes((IDu,N,I,M), (cuidado(_,_,IDu,IDp,_,_), utente(IDu,N,I,M)), S), apagaRepetidos(S,R).
identificaUtenteNomePrest(Np,R) :- solucoes((IDu,N,I,M), (prestador(IDp,Np,_,_), cuidado(_,_,IDu,IDp,_,_), utente(IDu,N,I,M)), S), apagaRepetidos(S,R).
identificaUtenteEsp(E,R) :- solucoes((IDu,N,I,M), (prestador(IDp,_,E,_), cuidado(_,_,IDu,IDp,_,_), utente(IDu,N,I,M)), S), apagaRepetidos(S,R).
identificaUtenteInst(IDi,R) :- solucoes((IDu,N,I,M), (prestador(IDp,_,_,IDi), cuidado(_,_,IDu,IDp,_,_), utente(IDu,N,I,M)), S), apagaRepetidos(S,R).
identificaUtenteNomeInst(Ni,R) :- solucoes((IDu,N,I,M), (instituicao(IDi,Ni,_), prestador(IDp,_,_,IDi), cuidado(_,_,IDu,IDp,_,_), utente(IDu,N,I,M)), S), apagaRepetidos(S,R).
identificaUtenteCidade(L,R) :- solucoes((IDu,N,I,M), (instituicao(IDi,_,L), prestador(IDp,_,_,IDi), cuidado(_,_,IDu,IDp,_,_), utente(IDu,N,I,M)), S), apagaRepetidos(S,R).

%Determinar todas as instituições/prestadores a que um utente já recorreu, devolvendo tambem a especialidade que procurou.
identificaInstPrestIDU(IDu,R) :- solucoes((IDi,Ni,IDp,Nome,Esp), (cuidado(D,H,IDu,IDp,De,C), prestador(IDp, Nome, Esp, IDi), instituicao(IDi,Ni,L)), S), apagaRepetidos(S,R).

%Determinar a soma dos elementos de uma lista
somaLista([],0).
somaLista([C],C).
somaLista([C|T],R) :- somaLista(T,N), R is N+C.

%Determinar a maior entre duas datas
dataMaior(Y1-M1-D1,Y2-M2-D2,Y2-M2-D2) :- Y2 > Y1; (Y2 == Y1, M2 > M1); (Y2 == Y1, M2 == M1, D2 >= D1).
dataMaior(Y1-M1-D1,Y2-M2-D2,Y1-M1-D1) :- Y2 < Y1; (Y2 == Y1, M2 < M1); (Y2 == Y1, M2 == M1, D2 < D1).

%Determinar lista de custos ocorridos entre determinadas datas
custoPorDatas([(D,C)],Di,Df,[]) :- \+dataMaior(Di,D,D); \+dataMaior(D,Df,Df).
custoPorDatas([(D,C)|T],Di,Df,[C|R]) :- dataMaior(Di,D,D), dataMaior(D,Df,Df), custoPorDatas(T,Di,Df,R).
custoPorDatas([(D,C)|T],Di,Df,R) :- (\+dataMaior(Di,D,D); \+dataMaior(D,Df,Df)), custoPorDatas(T,Di,Df,R).

%Calcular o custo total dos cuidados de saúde por critérios tais como utente/especialidade/prestador/datas
custoUtente(IDu,N) :- solucoes(C, cuidado(_,_,IDu,_,_,C), S), somaCusto(S,N).
custoEspecialidade(E,N) :- solucoes(C, (cuidado(_,_,_,IDp,_,C), prestador(IDp,_,E,_)), S), somaCusto(S,N).
custoPrestador(IDp,N) :- solucoes(C, cuidado(_,_,_,IDp,_,C), S), somaCusto(S,N).
custoInstituicao(IDi,N) :- solucoes(C, (cuidado(_,_,_,IDp,_,C), prestador(IDp,_,_,IDi)), S), somaCusto(S,N).
custoData(D,N) :- solucoes(C, cuidado(D,_,_,_,_,C), S), somaCusto(S,N).
custoDatas(Di,Df,N) :- solucoes((D,C), cuidado(D,_,_,_,_,C), S), custoPorDatas(S,Di,Df,R), somaCusto(R,N).

%Determinar o número de utentes/prestadores que receberam cuidados numa determinada instituição
nrUtentesInstituicao(IDi,N) :- solucoes(IDu, (prestador(IDp,_,_,IDi), cuidado(_,_,IDu,IDp,_,_)), S), apagaRepetidos(S,R), comprimento(R,N).
nrPrestadoresInstituicao(IDi,N) :- solucoes(IDp, prestador(IDp,_,_,IDi), S), apagaRepetidos(S,R), comprimento(R,N).
nrEspecialidadesInstituicao(IDi,N) :- solucoes(E, (prestador(IDp,_,E,IDi), cuidado(_,_,_,IDp,_,_)), S), apagaRepetidos(S,R), comprimento(R,N).

%Determinar o número de utentes que receberam cuidados de um determinado prestador
nrUtentesPrestador(IDp,N) :- solucoes(IDu, (prestador(IDp,_,_,_), cuidado(_,_,IDu,IDp,_,_)), S), apagaRepetidos(S,R), comprimento(R,N).

%Determinar o número de vezes que um elemento aparece numa lista
quantidade(X,[],(X,0)).
quantidade(X,[H|T],(X,N)) :- X == H, quantidade(X,T,(X,G)), N is G+1.
quantidade(X,[H|T],L) :- X \= H, quantidade(X,T,L).

%Criar uma lista em que a cada elemento está associado o número de vezes que este aparece numa lista inicial
listaTuplos([],[]).
listaTuplos([H|T],[L|R]) :- quantidade(H,[H|T],L), apagaT([H|T],H,Q), listaTuplos(Q,R).

%Determinar o tuplo da lista com maior valor
maiorElemento([(X,Y)],(X,Y)).
maiorElemento([(X,Y),(W,Z)|T],R) :- Y >= Z, maiorElemento([(X,Y)|T], R).
maiorElemento([(X,Y),(W,Z)|T],R) :- Y < Z, maiorElemento([(W,Z)|T], R).

%Determinar os N maiores elementos de uma lista
topN([],N,[]).
topN(L,0,[]).
topN(L,N,[X|R]) :- maiorElemento(L,(X,Y)), apagaT(L,(X,Y),S), M is N-1, topN(S,M,R).

%Associar a cada ID de prestador o seu nome
nomesPrestadores([],[]).
nomesPrestadores([ID|T],[(ID,N)|R]) :- prestador(ID,N,_,_), nomesPrestadores(T,R).

%Associar a cada ID de instituição o seu nome
nomesInstituicoes([],[]).
nomesInstituicoes([ID|T],[(ID,N)|R]) :- instituicao(ID,N,_), nomesInstituicoes(T,R).

%Determinar o top N de especialidades/prestadores/instituições mais ocorrentes nos cuidados
topNEspecialidades(N,R) :- solucoes(E, (prestador(IDp,_,E,_), cuidado(_,_,_,IDp,_,_)), S), listaTuplos(S,T), topN(T,N,R).
topNPrestadores(N,R) :- solucoes(IDp, (prestador(IDp,_,_,_), cuidado(_,_,_,IDp,_,_)), S), listaTuplos(S,T), topN(T,N,X), nomesPrestadores(X,R).
topNInstituicoes(N,R) :- solucoes(IDi, (prestador(IDp,_,_,IDi), cuidado(_,_,_,IDp,_,_)), S), listaTuplos(S,T), topN(T,N,X), nomesInstituicoes(X,R).

%Eliminar os tuplos de uma lista que tem como primeiro elemento um determinado X
apagaTuplos([],R,[]).
apagaTuplos([(X,Y)|T],X,R) :- apagaTuplos(T,X,R).
apagaTuplos([(X,Y)|T],N,[(X,Y)|R]) :- X \= N, apagaTuplos(T,N,R).

%Determinar a soma dos custos associados a determinado elemento
custoElemento(X,[],0).
custoElemento(X,[(X,Z)|T],R) :- custoElemento(X,T,G), R is G+Z.
custoElemento(X,[(Y,Z)|T],R) :- X \= Z, custoElemento(X,T,R).

%Determinar a soma dos custos associados a cada elemento da lista
custoTuplos([],[]).
custoTuplos([(X,Y)|T],[(X,C)|R]) :- custoElemento(X,[(X,Y)|T],C), apagaTuplos([(X,Y)|T],X,L), custoTuplos(L,R).

%Determinar as N especialidades/instituições mais lucrativas
especialidadesMaisLucrativas(N,R) :- solucoes((E,C), (prestador(IDp,_,E,_), cuidado(_,_,_,IDp,_,C)), S), custoTuplos(S,T), topN(T,N,R).
instituicoesMaisLucrativas(N,R) :- solucoes((Ni,C), (instituicao(IDi,Ni,_), prestador(IDp,_,_,IDi), cuidado(_,_,_,IDp,_,C)), S), custoTuplos(S,T), topN(T,N,R).

%Associar a cada ID de utente o seu nome
nomesUtentes([],[]).
nomesUtentes([ID|T],[(ID,N)|R]) :- utente(ID,N,_,_), nomesUtentes(T,R).

%Determinar os N utentes que mais dinheiro gastam em determinada instituição
utentesMaiorCustoInst(IDi,N,R) :- solucoes((IDu,C), (prestador(IDp,_,_,IDi), cuidado(_,_,IDu,IDp,_,C)), S), custoTuplos(S,T), topN(T,N,X), nomesUtentes(X,R).

%Determinar a média dos elementos de uma lista
mediaLista(L,M) :- somaLista(L,S), comprimento(L,T), M is S/T.

%Substituir o ID de utente pela sua idade
idadesUtentes([],[]).
idadesUtentes([ID|T],[I|R]) :- utente(ID,_,I,_), idadesUtentes(T,R).

%Determinar a média das idades dos utentes geral/por instituição/por especialidade
mediaIdadeGeral(R) :- solucoes(I, utente(_,_,I,_), S), mediaLista(S,R).
mediaIdadeInstituicao(IDi,R) :- solucoes(IDu, (prestador(IDp,_,_,IDi), cuidado(_,_,IDu,IDp,_,_)), S), apagaRepetidos(S,T), idadesUtentes(T,I), mediaLista(I,R).
mediaIdadeEspecialidade(E,R) :- solucoes(IDu, (prestador(IDp,_,E,_), cuidado(_,_,IDu,IDp,_,_)), S), apagaRepetidos(S,T), idadesUtentes(T,I), mediaLista(I,R).

%Verifica se a data de um determinado cuidado está entre duas datas.
verificaDataCuidado(D1,D2,Dc) :- dataMaior(D1,Dc,Dc),
								 dataMaior(D2,Dc,D2).

%Determina a lista dos utentes que requisitaram um cuidado de uma certa especialidade entre duas datas.
utentesEspecialidadeDatas(E,D1,D2,S) :- solucoes((IDu,Nu), (prestador(IDp,_,E,_), cuidado(Dc,_,IDu,IDp,_,_), verificaDataCuidado(D1,D2,Dc), utente(IDu,Nu,_,_)), S).

%Determina a lista dos utentes que consultaram um cuidado com determinado prestador entre duas datas.
utentesPrestadorDatas(IDp,D1,D2,S) :- solucoes((IDu,Nu), (prestador(IDp,_,_,_), cuidado(Dc,_,IDu,IDp,_,_), verificaDataCuidado(D1,D2,Dc), utente(IDu,Nu,_,_)), S).

%Determina a lista dos cuidados de uma determinada especialidade entre duas datas.
cuidadosPorEspecialidadeDatas(E,D1,D2,S) :- solucoes((Dc,H), (prestador(IDp,_,E,_), cuidado(Dc,H,_,IDp,_,_), verificaDataCuidado(D1,D2,Dc)), S).

%Determina a lista dos cuidados entre duas datas.
cuidadosEntreDatas(D1,D2,S) :- solucoes((Dc,H), (cuidado(Dc,H,_,_,_,_), verificaDataCuidado(D1,D2,Dc)), S).

%Determina a lista dos prestadores que realizaram cuidados entre duas datas.
prestadoresCuidadosEntreDatas(D1,D2,S) :- solucoes((IDp,Np), (prestador(IDp,Np,_,_), cuidado(Dc,_,_,IDp,_,_), verificaDataCuidado(D1,D2,Dc)), S).