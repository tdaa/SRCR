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

utente(1, tiago_alves, xpto1, vila_verde, 2018).
utente(2, isabel_pereira, 21, vila_verde, 2017).
utente(3, francisco_matos, 16, xpto7,2010).
utente(4, maria_teixeira, 21, gondizalves, 2011).
utente(5, rita_pereira, 45, xpto7, 2016).
utente(6, antonio_silva, xpto12, xpto13, 2009).
utente(7, mario_duarte, xpto6, gualtar, 1987).
utente(8, helena_dias, xpto6, amares, 1997).
utente(9, sara_pires, 6, xpto2, 2016).
utente(10, hugo_antunes, 83, xpto2,2008).

%Extensão do predicado prestador: #IdPres, Nome, Especialidade, #IdInst, AnoAtualização -> {V,F}

prestador(1, tania_fernandes, psiquiatria, xpto3, 2012).
prestador(2, mario_oliveira, clinica_geral, 2, 2010).
prestador(3, filipa_ferreira, pediatria, xpto3, 2017).
prestador(4, joao_machado, dermatologia, 2, 2014).
prestador(5, andre_correia, psiquiatria, xpto8, 2016).
prestador(6, renato_torres, pediatria, xpto8, 2018).
prestador(7, andreia_silva, cardiologia, 4, 2017).
prestador(8, andre_fernandes, neurologia, xpto8, 2016).
prestador(9, filipe_alves, oftalmologia, xpto8, 2001).
prestador(10, julio_goncalves, urologia, 1, 2018).
prestador(11, goncalo_matos, ginecologia, 1, 2010).
prestador(12, filipe_alves, clinica_geral, xpto8,2017).

%Extensão do predicado instituicao: #IdInst, Designacao, Localidade -> {V,F}

instituicao(1, hospital_de_braga, braga).
instituicao(2, hospital_da_trofa, braga).
instituicao(3, hospital_sao_joao, porto).
instituicao(4, hospital_santa_maria, porto).
instituicao(5, hospital_da_senhora_da_oliveira, guimaraes).

%Extensão do predicado cuidado: data, hora, #IdUt, #IdPrest, Descricao, Custo -> {V,F}

cuidado(2018-4-10, 16:00, xpto11, 1, ansiedade, 4).
cuidado(2018-4-11, 17:00, 5, 7, xpto4, 45).
cuidado(2018-5-16, 9:00, 2, 4, alergia_na_pele, xpto10).
cuidado(2018-5-30, 9:00, 2, 4, xpto4, 60).
cuidado(2018-6-2, 15:30, 3, 3, xpto9, 10).
cuidado(2018-9-30, 11:00, 3, 5, xpto21, xpto22).
cuidado(2018-10-20, 10:45, 1, 4, acne, 60).
cuidado(2018-10-22, 14:25, xpto14, 10, xpto15, 5).
cuidado(2018-11-5, 11:30, xpto16, 11, ecografia, xpto17).
cuidado(2018-11-5, 11:00, xpto18, 2, xpto19, xpto20).
cuidado(2018-11-27, 13:45, 6, 9, miopia, 55).
cuidado(2018-12-4, 12:00, 8, 8, xpto21, xpto22).

%Extensão do predicado que define a negação forte do utente: #IdUt, Nome, Idade, Morada, AnoAtualização -> {V,F}
-utente(ID,N,I,M,A) :- nao(utente(ID,N,I,M,A)), nao(excecao(utente(ID,N,I,M,A))).

%Extensão do predicado que define a negação forte do prestador: #IdPres, Nome, Especialidade, #IdInst, AnoAtualização -> {V,F}
-prestador(ID,N,E,IDi,A) :- nao(prestador(ID,N,E,IDi,A)), nao(excecao(prestador(ID,N,E,IDi,A))).

%Extensão do predicado que define a negação forte da instituicao: #IdInst, Designacao, Localidade -> {V,F}
-instituicao(ID,N,M) :- nao(instituicao(ID,N,M)), nao(excecao(instituicao(ID,N,M))).

%Extensão do predicado que define a negação forte do cuidado: Data, Hora, IdentificadorUtente, IdentificadorPrestador, Descrição, Custo -> {V,F}
-cuidado(D,H,IDu,IDp,Ds,C) :- nao(cuidado(D,H,IDu,IDp,Ds,C)), nao(excecao(cuidado(D,H,IDu,IDp,Ds,C))).

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

%Nulos interditos sobre o identificador da intituição dos prestadores e os respetivos Invariantes
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

%Conhecimento incerto sobre a idade e a morada de um utente
excecao(utente(Id,N,I,M,A)) :- utente(Id,N,xpto6,M,A).
excecao(utente(Id,N,I,M,A)) :- utente(Id,N,I,xpto7,A).
excecao(utente(Id,N,I,M,A)) :- utente(Id,N,xpto12,xpto13,A).

%Conhecimento incerto sobre o identificador da instituição de um prestador
excecao(prestador(Id,N,E,Idi,A)) :- prestador(Id,N,E,xpto8,A).

%Conhecimento incerto sobre a descricao, o custo e o identificador do utente dos cuidados
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,xpto9,C).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,Ds,xpto10).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,xpto11,IDp,Ds,C).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,xpto14,IDp,xpto15,C).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,xpto16,IDp,Ds,xpto17).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,xpto18,IDp,xpto19,xpto20).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,xpto21,xpto22).

%Invariante Estrutural: não permitir a inserção de conhecimento repetido

+instituicao(ID,D,C)  :: (solucoes(ID, instituicao(ID,X,Y), S),
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

%Invariante Referencial: verificar se utente e prestador existem na base de conhecimento, caso contrário não é permitida a adição do cuidado

+cuidado(D,H,IDu,IDp,DC,C) :: (solucoes(IDu, utente(IDu,_,_,_), S),
							   comprimento(S,L),
							   L==1
							   ).

+cuidado(D,H,IdU,Idp,DC,C) :: (solucoes(IDp, prestador(IDp,_,_,_), S),
							   comprimento(S,L),
							   L==1
							   ).