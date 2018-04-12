%Sistemas de Representação de Conhecimento e Raciocínio
%Exercício 1 - Prestação de Cuidados de Saúde

:- set_prolog_flag( discontiguous_warnings, off).
:- set_prolog_flag( single_var_warnings, off).
:- set_prolog_flag( unknown, fail).

%Definições iniciais

:- op(900, xfy, '::').
:- op(996, xfy, '&&').
:- op(997, xfy, '##').
:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic utente/5.
:- dynamic prestador/5.
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

%Extensão do predicado involucao

involucao(Termo) :- solucoes(Inv, -Termo :: Inv, S),
                     remove(Termo),
    				 teste(S).

%Extensão do meta-predicado demoLista: Lista_de_Questoes,Resposta -> {V,F}

demoLista([],[]).
demoLista([Q1|T],[R1|R]) :- demo(Q1,R1), demoLista(T,R).

%Extensão do predicado conjuncao: Valor,Valor,Resposta -> {V,F}

conjuncao(V1,V2,falso) :- V1 == falso; V2 == falso.
conjuncao(verdadeiro,verdadeiro,verdadeiro).
conjuncao(verdadeiro,desconhecido,desconhecido).
conjuncao(desconhecido,V,desconhecido) :- V == desconhecido; V == verdadeiro.

%Extensão do predicado disjuncao: Valor,Valor,Resposta -> {V,F}

disjuncao(V1,V2,verdadeiro) :- V1 == verdadeiro; V2 = verdadeiro.
disjuncao(falso,falso,falso).
disjuncao(falso,desconhecido,desconhecido).
disjuncao(desconhecido,V,desconhecido) :- V == desconhecido; V == falso.

%Extensão do meta-predicado demoComp: Composicao_de_Questoes,Resposta -> {V,F}

demoComp(Q1 && CQ, R) :- demo(Q1,R1), demoComp(CQ,R2), conjuncao(R1,R2,R).
demoComp(Q1 ## CQ, R) :- demo(Q1,R1), demoComp(CQ,R2), disjuncao(R1,R2,R).
demoComp(Q1, R) :- demo(Q1,R).

%Extensão do meta-predicado demo: Questao,Resposta -> {V,F}

demo(Questao,verdadeiro) :- Questao.
demo(Questao,falso) :- -Questao.
demo(Questao,desconhecido) :- nao(Questao), nao(-Questao).

%Extensão do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%Extensão do meta-predicado demoLista: Lista_de_Questoes,Resposta -> {V,F}

demoLista([],[]).
demoLista([Q1|T],[R1|R]) :- demo(Q1,R1), demoLista(T,R).

%Extensão do predicado utente: #IdUt, Nome, Idade, Morada, AnoAtualização -> {V,F}

utente(1, tiago_alves, 20, vila_verde, 2018).
utente(2, isabel_pereira, 21, vila_verde, 2017).
utente(3, francisco_matos, 16, tenoes,2010).
utente(4, maria_teixeira, 21, gondizalves, 2011).
utente(5, rita_pereira, 45, braga, 2016).
utente(6, antonio_silva, 33, guimaraes, 2009).
utente(7, mario_duarte, 64, gualtar, 1987).
utente(8, helena_dias, 75, amares, 1997).
utente(9, sara_pires, 6, povoa_de_lanhoso, 2016).
utente(10, hugo_antunes, 83, esposende,2008).

%Extensão do predicado prestador: #IdPres, Nome, Especialidade, #IdInst, AnoAtualização -> {V,F}

prestador(1, tania_fernandes, psiquiatria, 3, 2012).
prestador(2, mario_oliveira, clinica_geral, 2, 2010).
prestador(3, filipa_ferreira, pediatria, 1, 2017).
prestador(4, joao_machado, dermatologia, 2, 2014).
prestador(5, andre_correia, psiquiatria, 4, 2016).
prestador(6, renato_torres, pediatria, 4, 2018).
prestador(7, andreia_silva, cardiologia, 4, 2017).
prestador(8, andre_fernandes, neurologia, 2, 2016).
prestador(9, filipe_alves, oftalmologia, 3, 2001).
prestador(10, julio_goncalves, urologia, 1, 2018).
prestador(11, goncalo_matos, ginecologia, 1, 2010).
prestador(12, filipe_alves, clinica_geral, 2,2017).

%Extensão do predicado instituicao: #IdInst, Designacao, Localidade -> {V,F}

instituicao(1, hospital_de_braga, braga).
instituicao(2, hospital_da_trofa, braga).
instituicao(3, hospital_sao_joao, porto).
instituicao(4, hospital_santa_maria, porto).
instituicao(5, hospital_da_senhora_da_oliveira, guimaraes).

%Extensão do predicado cuidado: data, hora, #IdUt, #IdPrest, Descricao, Custo -> {V,F}

cuidado(2018-4-10, 16:00, 2, 1, ansiedade, 4).
cuidado(2018-4-11, 17:00, 5, 7, rotina, 45).
cuidado(2018-5-16, 9:00, 2, 4, alergia_na_pele, 60).
cuidado(2018-5-30, 9:00, 2, 4, alergia_na_pele, 60).
cuidado(2018-6-2, 15:30, 3, 3, checkup, 10).
cuidado(2018-9-30, 11:00, 3, 5, stress, 30).
cuidado(2018-10-20, 10:45, 1, 4, acne, 60).
cuidado(2018-10-22, 14:25, 10, 10, infecao, 5).
cuidado(2018-11-5, 11:30, 4, 11, ecografia, 35).
cuidado(2018-11-5, 11:00, 7, 2, rotina, 70).
cuidado(2018-11-27, 13:45, 6, 9, miopia, 55).
cuidado(2018-12-4, 12:00, 8, 8, dores_de_cabeca, 4).

%Extensão do predicado negativo utente: #IdUt, Nome, Idade, Morada, AnoAtualização -> {V,F}
-utente(ID,N,I,M,A) :- nao(utente(ID,N,I,M,A)), nao(excecao(utente(ID,N,I,M,A))).

%Extensão do predicado negativo prestador: #IdPres, Nome, Especialidade, #IdInst, AnoAtualização -> {V,F}
-prestador(ID,N,E,IDi,A) :- nao(prestador(ID,N,E,IDi,A)), nao(excecao(prestador(ID,N,E,IDi,A))).

%Extensão do predicado negativo instituicao: #IdInst, Designacao, Localidade -> {V,F}
-instituicao(ID,N,M) :- nao(instituicao(ID,N,M)), nao(excecao(instituicao(ID,N,M))).

%Extensão do predicado negativo cuidado: Data, Hora, IdentificadorUtente, IdentificadorPrestador, Descrição, Custo -> {V,F}
-cuidado(D,H,IDu,IDp,Ds,C) :- nao(cuidado(D,H,IDu,IDp,Ds,C)), nao(excecao(cuidado(D,H,IDu,IDp,Ds,C))).

%Invariante Estrutural: não permitir a inserção de conhecimento repetido

+instituicao(ID,D,C)  :: (solucoes(ID, instituicao(ID,X,Y), S),
					      comprimento(S,L),
					      L =< 1
					     ).

%Invariante Referencial: não permitir a remoção de conhecimento que esteja a ser utilizado

-instituicao(ID,N,M) :: (solucoes( ID, prestador(IDp,Np,E,ID), S),
						 comprimento(S,L),
						 L==0
					   	).

%Nulos interditos sobre a idade e a morada dos utentes e os respetivos Invariantes
nulo_utente_idade(xpto1).
excecao(utente(Id,N,I,M,A)) :- utente(Id,N,I,M,xpto1,A).

+utente(Id,N,I,M,A) :: (solucoes(Is,(utente(Id,_,Is,_,_), nulo_utente_idade(Is)),S),
						comprimento(S,L),
						L==0
					   ).

nulo_utente_morada(xpto2).
excecao(utente(Id,N,I,M,A)) :- utente(Id,N,I,xpto2,A).

+utente(Id,N,I,M,A) :: (solucoes(Ms,(utente(Id,_,_,Ms,_), nulo_utente_morada(Ms)),S),
						comprimento(S,L),
						L==0
					   ).

%Nulos interditos sobre a especialidade dos prestadores e os respetivos Invariantes
nulo_prestador_idInst(xpto3).
excecao(prestador(Id,N,E,Idi,A)) :- prestador(Id,N,E,xpto3,A).

+prestador(Id,N,E,Idi,A) :: (solucoes(Ids,(utente(Id,_,_,Ids,_), nulo_prestador_idInst(Ids)),S),
							 comprimento(S,L),
							 L==0
					   		).

%Nulos interditos sobre a descricao e o custo dos cuidados e os respetivos Invariantese
nulo_cuidado_descricao(xpto4).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,xpto4,C).

+cuidado(D,H,IDu,IDp,Ds,C) :: (solucoes(Dss,(cuidado(D,H,IDu,IDp,Dss,C), nulo_cuidado_descricao(Dss)),S),
							   comprimento(S,L),
							   L==0
					   		  ).

nulo_cuidado_custo(xpto5).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,Ds,xpto5).

+cuidado(D,H,IDu,IDp,Ds,C) :: (solucoes(Cs,(cuidado(D,H,IDu,IDp,Ds,Cs), nulo_cuidado_custo(Cs)),S),
							   comprimento(S,L),
							   L==0
					   		  ).

%Conhecimento Incerto sobre a idade e a morada dos utentes
excecao(utente(Id,N,I,M,A)) :- utente(Id,N,xpto6,M,A).

excecao(utente(Id,N,I,M,A)) :- utente(Id,N,I,xpto7,A).

excecao(prestador(Id,N,E,Idi,A)) :- prestador(Id,N,E,xpto8,A).

excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,xpto9,C).

excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,Ds,xpto10).

excecao(cuidad(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,xpto11,IDp,Ds,C).