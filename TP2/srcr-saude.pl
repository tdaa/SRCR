%Sistemas de Representação de Conhecimento e Raciocínio
%Exercício 1 - Prestação de Cuidados de Saúde

:- set_prolog_flag( discontiguous_warnings, off).
:- set_prolog_flag( single_var_warnings, off).
:- set_prolog_flag( unknown, fail).

%Definições iniciais

:- op(900, xfy, '::').
:- op(996, xfy, '&&').
:- op(997, xfy, '##').
:- op(998, xfy, '~').
:- op(999, xfy, '^').
:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic utente/5.
:- dynamic prestador/5.
:- dynamic instituicao/3.
:- dynamic cuidado/6.
:- dynamic utente_perfeito/1.
:- dynamic utente_impreciso_idade/1.
:- dynamic utente_impreciso_morada/1.
:- dynamic prestador_perfeito/1.
:- dynamic prestador_impreciso_idInst/1.
:- dynamic cuidado_perfeito/3.
:- dynamic cuidado_impreciso_preco/3.
:- dynamic cuidado_impreciso_descricao/3.
:- dynamic cuidado_impreciso_idUt/3.

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
utente_perfeito(2).
utente(3, francisco_matos, 16, xpto7, 2010).
utente(4, maria_teixeira, 21, gondizalves, 2011).
utente_perfeito(4).
-utente(5, rita_pereira, 45, maximinos, 2016).
utente(5, rita_pereira, 45, xpto7, 2016).
-utente(6, antonio_silva, 12, xpto13, 2009).
-utente(6, antonio_silva, xpto12, horta, 2009).
utente(6, antonio_silva, xpto12, xpto13, 2009).
utente(7, mario_duarte, xpto6, gualtar, 1987).
utente(8, helena_dias, 19, amares, 1997).
utente_perfeito(8).
utente(9, sara_pires, 6, xpto2, 2016).
utente(10, hugo_antunes, 83, xpto2, 2008).
excecao(utente(11, rita_dias, 15, ferreiros, 2010)).
excecao(utente(11, rita_dias, 16, ferreiros, 2010)).
utente_impreciso_idade(11).
excecao(utente(12, luisa_fernandes, xpto6, gondomar, 2012)).
excecao(utente(12, luisa_fernandes, xpto6, boavista, 2012)).
utente_impreciso_morada(12).
excecao(utente(13, ricardo_pires, 32, barros, 2013)).
excecao(utente(13, ricardo_pires, 32, olhao, 2013)).
excecao(utente(13, ricardo_pires, 33, olhao, 2013)).
excecao(utente(13, ricardo_pires, 33, barros, 2013)).
utente_impreciso_idade(13).
utente_impreciso_morada(13).
-utente(14, maria_felgueiras, 25, gondomar, 2010).
utente_perfeito(14). 

%Extensão do predicado prestador: #IdPres, Nome, Especialidade, #IdInst, AnoAtualização -> {V,F}

prestador(1, tania_fernandes, psiquiatria, xpto3, 2012).
prestador(2, mario_oliveira, clinica_geral, 2, 2010).
prestador_perfeito(2).
prestador(3, filipa_ferreira, pediatria, xpto3, 2017).
prestador(4, joao_machado, dermatologia, 2, 2014).
prestador_perfeito(4).
-prestador(5, andre_correia, psiquiatria, 4, 2016).
prestador(5, andre_correia, psiquiatria, xpto8, 2016).
prestador(6, renato_torres, pediatria, xpto8, 2018).
prestador(7, andreia_silva, cardiologia, 4, 2017).
prestador_perfeito(7).
prestador(8, andre_fernandes, neurologia, xpto8, 2016).
prestador(9, filipe_alves, oftalmologia, xpto8, 2001).
-prestador(10, julio_goncalves, urologia, 1, 2018).
prestador_perfeito(10).
-prestador(11, goncalo_matos, ginecologia, 1, 2010).
prestador_perfeito(11).
prestador(12, filipe_alves, clinica_geral, xpto8,2017).
excecao(prestador(13, nuno_oliveira, oftamologia, 2, 2013)).
excecao(prestador(13, nuno_oliveira, oftamologia, 3, 2013)).
prestador_impreciso_idInst(13).

%Extensão do predicado instituicao: #IdInst, Designacao, Localidade -> {V,F}

instituicao(1, hospital_de_braga, braga).
instituicao(2, hospital_da_trofa, braga).
instituicao(3, hospital_sao_joao, porto).
instituicao(4, hospital_santa_maria, porto).
instituicao(5, hospital_da_senhora_da_oliveira, guimaraes).

%Extensão do predicado cuidado: data, hora, #IdUt, #IdPrest, Descricao, Custo -> {V,F}

-cuidado(2018-4-10, 16:00, 1, 1, ansiedade, 4).
cuidado(2018-4-10, 16:00, xpto11, 1, ansiedade, 4).
cuidado(2018-4-11, 17:00, 5, 7, xpto4, 45).
cuidado(2018-5-16, 9:00, 2, 4, alergia_na_pele, xpto10).
cuidado(2018-5-30, 9:00, 2, 4, xpto4, 60).
cuidado(2018-6-2, 15:30, 3, 3, xpto9, 10).
cuidado(2018-9-30, 11:00, 3, 5, xpto21, xpto22).
cuidado(2018-10-20, 10:45, 1, 4, acne, 60).
cuidado_perfeito(2018-10-20, 10:45, 4).
cuidado(2018-10-22, 14:25, xpto14, 10, xpto15, 5).
cuidado(2018-11-5, 11:30, xpto16, 11, ecografia, xpto17).
cuidado(2018-11-5, 11:00, xpto18, 2, xpto19, xpto20).
cuidado(2018-11-27, 13:45, 6, 9, miopia, 55).
cuidado_perfeito(2018-11-27, 13:45, 9).
cuidado(2018-12-4, 12:00, 8, 8, xpto21, xpto22).
excecao(cuidado(2018-12-8, 7:00, 10, 8, dores_de_cabeca, C)) :- C =< 39, C >= 35.
cuidado_impreciso_preco(2018-12-8, 7:00, 8).
excecao(cuidado(2018-12-21, 21:00, 11, 12, checkup, 5)).
excecao(cuidado(2018-12-21, 21:00, 11, 12, febre, 5)).
cuidado_impreciso_descricao(2018-12-21, 21:00, 12).
excecao(cuidado(2018-12-28, 15:00, 9, 7, ataque_cardiaco, 38)).
excecao(cuidado(2018-12-28, 15:00, 13, 7, ataque_cardiaco, 38)).
cuidado_impreciso_idUt(2018-12-28, 15:00, 7).
-cuidado(2019-01-05, 17:45, 3, 5, esquizofrenia, 10).
cuidado_perfeito(2019-01-05, 17:45, 5).
-cuidado(2019-01-05, 17:45, 6, 8, epilepsia, 60).
cuidado_perfeito(2019-01-05, 17:45, 8).

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
						L==0).

nulo_utente_morada(xpto2).
excecao(utente(Id,N,I,M,A)) :- utente(Id,N,I,xpto2,A).

+utente(Id,N,I,M,A) :: (solucoes(Ms,(utente(Id,_,_,Ms,_), nulo_utente_morada(Ms)),S),
						comprimento(S,L),
						L==0).

%Nulos interditos sobre o identificador da intituição dos prestadores e os respetivos Invariantes
nulo_prestador_idInst(xpto3).
excecao(prestador(Id,N,E,Idi,A)) :- prestador(Id,N,E,xpto3,A).

+prestador(Id,N,E,Idi,A) :: (solucoes(Ids,(prestador(Id,_,_,Ids,_), nulo_prestador_idInst(Ids)),S),
							 comprimento(S,L),
							 L==0).

%Nulos interditos sobre a descricao e o custo dos cuidados e os respetivos Invariantese
nulo_cuidado_descricao(xpto4).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,xpto4,C).

+cuidado(D,H,IDu,IDp,Ds,C) :: (solucoes(Dss,(cuidado(D,H,IDu,IDp,Dss,C), nulo_cuidado_descricao(Dss)),S),
							   comprimento(S,L),
							   L==0).

nulo_cuidado_custo(xpto5).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,Ds,xpto5).

+cuidado(D,H,IDu,IDp,Ds,C) :: (solucoes(Cs,(cuidado(D,H,IDu,IDp,Ds,Cs), nulo_cuidado_custo(Cs)),S),
							   comprimento(S,L),
							   L==0).

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
					      L =< 1).

%Invariante Referencial: não permitir a remoção de conhecimento que esteja a ser utilizado

-utente(ID,N,I,M,D) :: (solucoes( ID, cuidado(A,H,ID,IDp,D,C), S),
					   comprimento(S,L),
					   L==0).

-prestador(ID,N,E,IDi,D) :: (solucoes( ID, cuidado(A,H,IDu,ID,D,C), S),
						    comprimento(S,L),
						    L==0).

-instituicao(ID,N,M) :: (solucoes( ID, prestador(IDp,Np,E,ID), S),
						 comprimento(S,L),
						 L==0).

%Invariante Referencial: verificar se utente e prestador existem na base de conhecimento, caso contrário não é permitida a adição do cuidado

+cuidado(D,H,IDu,IDp,DC,C) :: (solucoes(IDu, (utente(IDu,_,_,_,_); (excecao(utente(IDu,_,I,M,_)), I \= xpto6, I \= xpto12, M \= xpto7)), S),
							   comprimento(S,L),
							   L==1).

+cuidado(D,H,IDu,IDp,DC,C) :: (solucoes(IDp, (prestador(IDp,_,_,_,_); (excecao(prestador(IDp,_,_,Idi,_)), Idi \= 8)), S),
							   comprimento(S,L),
							   L==1).

%Extensão do predicado evolucao que só é utilizado para a instituicao

evolucao(Termo) :- solucoes(Inv, +Termo :: Inv, S),
				   insere(Termo), 
				   teste(S).

%Extensão do predicado involucao que pode ser utilizado para todos os predicados

involucao(Termo) :- solucoes(Inv, -Termo :: Inv, S),
      			    remove(Termo),
					teste(S).


+utente(ID,N,I,M,D) :: (solucoes(ID, (utente(ID,N,_,_,Ds), utente_perfeito(ID), abaixo1ano(D,Ds)),S),
						comprimento(S,L),
						L=<1).

+utente(ID,N,I,M,D) :: (solucoes(ID, (utente_perfeito(ID), -utente(ID,_,_,_,_)), S),
						comprimento(S,L),
						L==0).

+utente(ID,N,I,M,D) :: (solucoes(ID, ((utente(ID,N,_,_,Ds); excecao(utente(ID,N,_,_,Ds))), D < Ds), S),
						comprimento(S,L),
						L==0).

+utente(ID,N,I,M,D) :: (solucoes(ID, (utente(ID,N,Is,Ms,D), 
						((Is == xpto6, -utente(ID,N,In,Ms,D), I == In); 
						(Ms == xpto7, -utente(ID,N,Is,Mn,D), M == Mn);
						(Is == xpto12, -utente(ID,N,In,Mn,D), (I == In; M == Mn)))), S),
						comprimento(S,L),
						L==0).

+prestador(ID,N,E,IDi,D) :: (solucoes(ID, (prestador(ID,N,E,_,Ds), prestador_perfeito(ID), abaixo4anos(D,Ds)),S),
							 comprimento(S,L),
							 L=<1).

+prestador(ID,N,E,IDi,D) :: (solucoes(ID, (prestador_perfeito(ID), -prestador(ID,_,_,_,_)), S),
							 comprimento(S,L),
							 L==0).

+prestador(ID,N,I,M,D) :: (solucoes(ID, ((prestador(ID,N,E,_,Ds); excecao(prestador(ID,N,E,_,Ds))), D < Ds), S),
						   comprimento(S,L),
						   L==0).

+prestador(ID,N,E,IDi,D) :: (solucoes(ID, (prestador(ID,N,E,IDs,D), 
							 IDs == xpto8, -prestador(ID,N,E,IDn,D), IDi == IDn), S),
							 comprimento(S,L),
							 L==0).

+cuidado(D,H,IDu,IDp,Ds,C) :: (solucoes((D,H,IDp), (cuidado(D,H,_,IDp,_,_), cuidado_perfeito(D,H,IDp)),S),
							   comprimento(S,L),
							   L=<1).

+cuidado(D,H,IDu,IDp,Ds,C) :: (solucoes(ID, (cuidado_perfeito(D,H,IDp), -cuidado(D,H,_,IDp,_,_)), S),
							  comprimento(S,L),
							  L==0).

+cuidado(D,H,IDu,IDp,Ds,C) :: (solucoes(ID, (cuidado(D,H,IDus,IDp,Dss,Cs), 
							  ((IDus == xpto11, -cuidado(D,H,IDun,IDp,Dss,Cs), IDu == IDun);
							  (IDus == xpto14, -cuidado(D,H,IDun,IDp,Dsn,Cs), (IDu == IDun; Ds == Dsn));
							  (IDus == xpto16, -cuidado(D,H,IDun,IDp,Dss,Cn), (IDu == IDun; C == Cn));
							  (IDus == xpto18, -cuidado(D,H,IDun,IDp,Dsn,Cn), (IDu == IDun; Ds == Dsn; C == Cn));
							  (Dss == xpto9, -cuidado(D,H,IDus,IDp,Dsn,Cs), Ds == Dsn);
							  (Dss == xpto21, -cuidado(D,H,IDus,IDp,Dsn,Cn), (Ds == Dsn; C == Cn));
							  (Cs == xpto10, -cuidado(D,H,IDus,IDp,Dss,Cn), C == Cn))), S),
							  comprimento(S,L),
							  L==0).

evolucaoPositiva(utente(ID,N,I,M,D)) :- solucoes(Inv, +utente(ID,N,I,M,D) :: Inv, S),
										insere(utente(ID,N,I,M,D)),
										teste(S),
										removeImpreciso(utente(ID,_,_,_,_)),
										removeIncerto(utente(ID,_,_,_,_)),
										removePerfeitoDatas(utente(ID,_,_,_,D)),
										assert(utente_perfeito(ID)).

evolucaoPositiva(prestador(ID,N,E,IDi,D)) :- solucoes(Inv, +prestador(ID,N,E,IDi,D) :: Inv, S),
											 insere(prestador(ID,N,E,IDi,D)),
											 teste(S),
											 removeImpreciso(prestador(ID,_,_,_,_)),
										  	 removeIncerto(prestador(ID,_,_,_,_)),
											 removePerfeitoDatas(prestador(ID,_,_,_,D)),
											 assert(prestador_perfeito(ID)).

evolucaoPositiva(cuidado(D,H,IDu,IDp,Ds,C)) :- solucoes(Inv, +cuidado(D,H,IDu,IDp,Ds,C) :: Inv, S),
											   insere(cuidado(D,H,IDu,IDp,Ds,C)),
											   teste(S),
											   removeImpreciso(cuidado(D,H,_,IDp,_,_)),
										  	   removeIncerto(cuidado(D,H,_,IDp,_,_)),
											   assert(cuidado_perfeito(D,H,IDp)).

+(-utente(ID,N,I,M,D)) :: (solucoes(ID, (utente(ID,_,Is,Ms,_), Is \= xpto6, Is \= xpto12, Ms \= xpto7), S),
						   comprimento(S,L),
						   L==0).

+(-utente(ID,N,I,M,D)) :: (solucoes(ID, (utente_impreciso_idade(ID); utente_impreciso_morada(ID)), S),
						   comprimento(S,L),
						   L==0).

+(-utente(ID,N,I,M,D)) :: (solucoes(ID, (-utente(ID,N,I,M,D)), S),
						   comprimento(S,L),
						   L=<2).

+(-utente(ID,N,I,M,D)) :: (solucoes(Is,(utente(ID,_,Is,_,_), nulo_utente_idade(Is)),S),
						   comprimento(S,L),
						   L==0).

+(-utente(ID,N,I,M,D)) :: (solucoes(Ms,(utente(ID,_,_,Ms,_), nulo_utente_morada(Ms)),S),
						   comprimento(S,L),
						   L==0).

+(-prestador(ID,N,E,IDi,D)) :: (solucoes(ID, (prestador(ID,_,_,IDs,_), IDs \= xpto8), S),
						   		comprimento(S,L),
						   		L==0).

+(-prestador(ID,N,E,IDi,D)) :: (solucoes(ID, prestador_impreciso_idInst(ID), S),
						   		comprimento(S,L),
						  		L==0).

+(-prestador(ID,N,E,IDi,D)) :: (solucoes(ID, -prestador(ID,N,E,IDi,D), S),
						   		comprimento(S,L),
						   		L=<2).

+(-prestador(ID,N,E,IDi,D)) :: (solucoes(IDs,(prestador(ID,_,_,IDs,_), nulo_prestador_idInst(IDs)),S),
						   		comprimento(S,L),
						   		L==0).

+(-cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,IDus,IDp,Dss,Cs), IDus \= xpto11, IDus \= xpto14, IDus \= xpto14, IDus \= xpto16, IDus \= xpto18, Dss \= xpto9, Dss \= 21, Cs \= xpto11), S),
						   		  comprimento(S,L),
						   		  L==0).

+(-cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes((D,H,IDp), (cuidado_impreciso_descricao(D,H,IDp); cuidado_impreciso_preco(D,H,IDp); cuidado_impreciso_idUt(D,H,IDp)), S),
						   		  comprimento(S,L),
						  		  L==0).

+(-cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, -cuidado(D,H,IDu,IDp,Ds,C), S),
						   		  comprimento(S,L),
						   		  L=<2).

+(-cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(IDs,(cuidado(D,H,IDus,IDp,Ds,Cs), nulo_cuidado_custo(Cs), nulo_cuidado_descricao(Ds)),S),
						   		  comprimento(S,L),
						   		  L==0).

evolucaoNegativa(utente(ID,N,I,M,D)) :- solucoes(Inv, +(-utente(ID,N,I,M,D)) :: Inv, S),
									    insere(-utente(ID,N,I,M,D)),
										teste(S),
										solucoes(ID, (utente(ID,N,Is,Ms,D), (Is == xpto6; Is == xpto12; Ms == xpto7)), R),
										testaPerfeito(R,utente(ID,_,_,_,_)).

evolucaoNegativa(prestador(ID,N,E,IDi,D)) :- solucoes(Inv, +(-prestador(ID,N,E,IDi,D)) :: Inv, S),
									     	 insere(-prestador(ID,N,E,IDi,D)),
										 	 teste(S),
										 	 solucoes(ID, (prestador(ID,N,E,IDs,D), IDs == xpto8), R),
										 	 testaPerfeito(R,prestador(ID,_,_,_,_)).

evolucaoNegativa(cuidado(D,H,IDu,IDp,Ds,C)) :- solucoes(Inv, +(-cuidado(D,H,IDu,IDp,Ds,C)) :: Inv, S),
									     	   insere(-cuidado(D,H,IDu,IDp,Ds,C)),
										 	   teste(S),
										 	   solucoes(ID, (cuidado(D,H,IDu,IDp,Ds,C), IDu == xpto11; IDu == xpto14; IDu == xpto14; IDu == xpto16; IDu == xpto18; Ds == xpto9; Ds == 21; C == xpto11), R),
										 	   testaPerfeito(R,cuidado(D,H,_,IDp,_,_)).

~(utente(ID,N,I,M,D)) :: (solucoes(ID, (utente(ID,_,Is,Ms,_), Is \= xpto6, Is \= xpto12, Ms \= xpto7), S),
						  comprimento(S,L),
						  L==0).

~(utente(ID,N,I,M,D)) :: (solucoes(ID, -utente(ID,_,_,_,_), S),
						  comprimento(S,L),
						  L==0).

~(utente(ID,N,I,M,D)) :: (solucoes(ID, (utente(ID,_,Is,_,_), nulo_utente_idade(Is)), S),
						  comprimento(S,L),
						  L==0).

~(utente(ID,N,I,M,D)) :: (solucoes(ID, (utente(ID,_,_,Ms,_), nulo_utente_morada(Ms)), S),
						  comprimento(S,L),
						  L==0).

~(prestador(ID,N,E,IDi,D)) :: (solucoes(ID, (prestador(ID,_,_,IDs,_), IDs \= xpto8), S),
							   comprimento(S,L),
							   L==0).

~(prestador(ID,N,E,IDi,D)) :: (solucoes(ID, -prestador(ID,_,_,_,_), S),
							   comprimento(S,L),
							   L==0).

~(prestador(ID,N,E,IDi,D)) :: (solucoes(ID, (prestador(ID,_,_,IDs,_), nulo_prestador_idInst(IDs)), S),
							   comprimento(S,L),
							   L==0).

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,IDus,IDp,Dss,Cs), 
								 IDus \= xpto11, IDus \= xpto14, IDus \= xpto16, IDus \= xpto18,
								 Dss \= xpto9, Dss \= xpto21, Cs \= xpto10), S),
							     comprimento(S,L),
							     L==0).

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, -cuidado(D,H,_,IDp,_,_), S),
							     comprimento(S,L),
							     L==0).

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,_,IDp,Dss,_), nulo_cuidado_descricao(Dss)), S),
							     comprimento(S,L),
							     L==0).

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,_,IDp,_,Cs), nulo_cuidado_custo(Cs)), S),
							     comprimento(S,L),
							     L==0).

evolucaoImprecisa(utente(ID,N,I,M,D),T) :- solucoes(Inv, ~(utente(ID,N,I,M,D)) :: Inv, S),
										   insere(excecao(utente(ID,N,I,M,D))),
										   teste(S),
										   removeIncertoEspecifico(utente(ID,_,_,_,_),T),
										   (((T==i; T==im), assert(utente_impreciso_idade(ID))); 
										   ((T==m; T==im), assert(utente_impreciso_morada(ID)))).

evolucaoImprecisa(prestador(ID,N,E,IDi,D)) :- solucoes(Inv, ~(prestador(ID,N,E,IDi,D)) :: Inv, S),
										   	  insere(excecao(prestador(ID,N,E,IDi,D))),
										      teste(S),
										      removeIncerto(prestador(ID,_,_,_,_)),
										      assert(prestador_impreciso_idInst(ID)).

evolucaoImprecisa(cuidado(D,H,IDu,IDp,Ds,C),T) :- solucoes(Inv, ~(cuidado(D,H,IDu,IDp,Ds,C)) :: Inv, S),
										   	  	  insere(excecao(cuidado(D,H,IDu,IDp,Ds,C))),
										      	  teste(S),
										      	  removeIncertoEspecifico(cuidado(ID,_,_,_,_),T),
										      	  (((T==idu; T==idud; T==iduc; T==idudc), assert(cuidado_impreciso_idUt(D,H,IDp)));
										      	  ((T==d; T==idud; T==dc; T==idudc), assert(cuidado_impreciso_descricao(D,H,IDp)));
										      	  ((T==c; T==iduc; T==dc; T==idudc), assert(cuidado_impreciso_idUt(D,H,IDp)))).

^(utente(ID,N,I,M,D)) :: (solucoes(Is,(utente(ID,_,Is,_,_), nulo_utente_idade(Is)),S),
						  comprimento(S,L),
						  L==0).

^(utente(ID,N,I,M,D)) :: (solucoes(Ms,(utente(ID,_,_,Ms,_), nulo_utente_morada(Ms)),S),
						  comprimento(S,L),
						  L==0).

^(prestador(ID,N,E,IDi,D)) :: (solucoes(IDs,(prestador(ID,_,_,IDs,_), nulo_prestador_idInst(IDs)),S),
						   	   comprimento(S,L),
						   	   L==0).

^(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,_,IDp,Dss,_), nulo_cuidado_descricao(Dss)), S),
							     comprimento(S,L),
							     L==0).

^(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,_,IDp,_,Cs), nulo_cuidado_custo(Cs)), S),
							     comprimento(S,L),
							     L==0).

evolucaoInterdita(utente(ID,N,nulo,M,D)) :- solucoes(Inv, ^(utente(ID,N,xpto1,M,D)) :: Inv, S),
										 	insere(utente(ID,N,xpto1,M,D)),
										 	teste(S),
										 	removeImpreciso(utente(ID,_,_,_,_)),
										 	removeIncerto(utente(ID,_,_,_,_)),
										 	removePerfeito(utente(ID,_,_,_,_)).

evolucaoInterdita(utente(ID,N,I,nulo,D)) :- solucoes(Inv, ^(utente(ID,N,I,xpto2,D)) :: Inv, S),
										 	insere(utente(ID,N,I,xpto2,D)),
										 	teste(S),
										 	removeImpreciso(utente(ID,_,_,_,_)),
										 	removeIncerto(utente(ID,_,_,_,_)),
										 	removePerfeito(utente(ID,_,_,_,_)).

evolucaoInterdita(utente(ID,N,nulo,nulo,D)) :- solucoes(Inv, ^(utente(ID,N,xpto1,xpto2,D)) :: Inv, S),
										 	   insere(utente(ID,N,xpto1,xpto2,D)),
										 	   teste(S),
										 	   removeImpreciso(utente(ID,_,_,_,_)),
										       removeIncerto(utente(ID,_,_,_,_)),
										 	   removePerfeito(utente(ID,_,_,_,_)).

evolucaoInterdita(prestador(ID,N,E,nulo,D)) :- solucoes(Inv, ^(prestador(ID,N,E,xpto3,D)) :: Inv, S),
											   insere(prestador(ID,N,E,xpto3,D)),
											   teste(S),
											   removeImpreciso(prestador(ID,_,_,_,_)),
										  	   removeIncerto(prestador(ID,_,_,_,_)),
											   removePerfeito(prestador(ID,_,_,_,_)).

evolucaoInterdita(cuidado(D,H,IDu,IDp,nulo,C)) :- solucoes(Inv, ^(cuidado(D,H,IDu,IDp,xpto4,C)) :: Inv, S),
											   	  insere(cuidado(D,H,IDu,IDp,xpto4,C)),
											   	  teste(S),
											      removeImpreciso(cuidado(D,H,_,IDp,_,_)),
										  	      removeIncerto(cuidado(D,H,_,IDp,_,_)),
											      removePerfeito(cuidado(D,H,_,IDp,_,_)).

evolucaoInterdita(cuidado(D,H,IDu,IDp,Ds,nulo)) :- solucoes(Inv, ^(cuidado(D,H,IDu,IDp,Ds,xpto5)) :: Inv, S),
											   	   insere(cuidado(D,H,IDu,IDp,Ds,xpto5)),
											   	   teste(S),
											       removeImpreciso(cuidado(D,H,_,IDp,_,_)),
										  	       removeIncerto(cuidado(D,H,_,IDp,_,_)),
											       removePerfeito(cuidado(D,H,_,IDp,_,_)).

evolucaoInterdita(cuidado(D,H,IDu,IDp,nulo,nulo)) :- solucoes(Inv, ^(cuidado(D,H,IDu,IDp,xpto4,xpto5)) :: Inv, S),
											   	     insere(cuidado(D,H,IDu,IDp,xpto4,xpto5)),
											   	     teste(S),
											         removeImpreciso(cuidado(D,H,_,IDp,_,_)),
										  	         removeIncerto(cuidado(D,H,_,IDp,_,_)),
											         removePerfeito(cuidado(D,H,_,IDp,_,_)).

%PREDICADOS EXTRA

abaixo1ano(D1,D2) :- (D1-D2) < 1.

abaixo4anos(D1,D2) :- (D1-D2) < 4.

utentesAnomalias(S) :- solucoes((ID,N,I,M,D), (utente(ID,N,I,M,D), 
					   (utente_impreciso_idade(ID); utente_impreciso_morada(ID); 
					   nulo_utente_idade(ID); nulo_utente_morada(M); 
					   I == xpto6; I == xpto12; M == xpto7)), 
					   S).

prestadoresAnomalias(S) :- solucoes((ID,N,E,IDi,D), (prestador(ID,N,E,IDi,D),
						   (prestador_impreciso_idInst(ID); 
						   nulo_prestador_idInst(IDi);
						   IDi == xpto8)),
						   S).

removeLista([]).
removeLista([H|T]) :- retract(H), removeLista(T).


removeIncerto(utente(ID,_,_,_,_)) :- solucoes(utente(ID,N,I,M,D), (utente(ID,N,I,M,D), (I == xpto6; I == xpto12; M == xpto7)), S),
									 solucoes(-utente(ID,N,I,M,Ds), -utente(ID,N,I,M,Ds), R),
									 removeLista(S),
									 removeLista(R).
removeIncerto(prestador(ID,_,_,_,_)) :- solucoes(prestador(ID,N,E,IDi,D), (prestador(ID,N,E,IDi,D), IDi == xpto8), S),
									 	solucoes(-prestador(ID,N,E,IDi,Ds), -prestador(ID,N,E,IDi,Ds), R),
									 	removeLista(S),
									 	removeLista(R).
removeIncerto(cuidado(D,H,_,IDp,_,_)) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), 
										 (cuidado(D,H,IDu,IDp,Ds,C), 
										 (IDu == xpto11; IDu == xpto14; IDu == xpto16; IDu == xpto18; Ds == xpto9; Ds == xpto21; C == xpto10)), S),
									 	 solucoes(-cuidado(D,H,IDu,IDp,Ds,C), -cuidado(D,H,IDu,IDp,Ds,C), R),
									 	 removeLista(S),
									 	 removeLista(R).

removeImpreciso(utente(ID,_,_,_,_)) :- solucoes(utente(ID,N,I,M,D), (excecao(utente(ID,N,I,M,D)), I \= xpto6, I \= xpto12, M \= xpto7), S),
									   removeListaImprecisos(S).
removeImpreciso(prestador(ID,_,_,_,_)) :- solucoes(prestador(ID,N,E,IDi,D), (excecao(prestador(ID,N,E,IDi,D)), IDi \= xpto8), S),
									   	  removeListaImprecisos(S).
removeImpreciso(cuidado(D,H,_,IDp,_,_)) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (excecao(cuidado(D,H,IDu,IDp,Ds,C)), 
										   IDu \= xpto11, IDu \= xpto14, IDu \= xpto16, IDu \= xpto18, Ds \= xpto9, Ds \= xpto21, C \= xpto10), S),
									   	   removeListaImprecisos(S).

removePerfeitoDatas(utente(ID,_,_,_,_)) :- solucoes(utente(ID,N,I,M,D), (utente_perfeito(ID), utente(ID,N,I,M,D), Ds \= D), S),
									  	   solucoes(-utente(ID,N,I,M,D), (utente_perfeito(ID), -utente(ID,N,I,M,D)), R),
									  	   removeLista(S),
									  	   removeLista(R),
									  	   comprimento(S,C1),
									  	   comprimento(S,C2),
									  	   ((C1 \= 0; C2 \= 0), retract(utente_perfeito(ID))).

removePerfeitoDatas(prestador(ID,_,_,_,_)) :- solucoes(prestador(ID,N,E,IDi,D), (prestador_perfeito(ID), prestador(ID,N,E,IDi,D), Ds \= D), S),
										 	  solucoes(-prestador(ID,N,E,IDi,D), (prestador_perfeito(ID), -prestador(ID,N,E,IDi,D)), R),
									  	 	  removeLista(S),
									  	 	  removeLista(R),
									  	 	  comprimento(S,C1),
									  	 	  comprimento(S,C2),
									  	 	  ((C1 \= 0; C2 \= 0), retract(prestador_perfeito(ID))).

removePerfeito(utente(ID,_,_,_,_)) :- solucoes(utente(ID,N,I,M,Ds), (utente_perfeito(ID), utente(ID,N,I,M,Ds)), S),
									  solucoes(-utente(ID,N,I,M,Ds), (utente_perfeito(ID), -utente(ID,N,I,M,Ds)), R),
									  removeLista(S),
									  removeLista(R),
									  comprimento(S,C1),
									  comprimento(S,C2),
									  ((C1 \= 0; C2 \= 0), retract(utente_perfeito(ID))).

removePerfeito(prestador(ID,_,_,_,_)) :- solucoes(prestador(ID,N,E,IDi,D), (prestador_perfeito(ID), prestador(ID,N,E,IDi,D)), S),
										 solucoes(-prestador(ID,N,E,IDi,D), (prestador_perfeito(ID), -prestador(ID,N,E,IDi,D)), R),
									  	 removeLista(S),
									  	 removeLista(R),
									  	 comprimento(S,C1),
									  	 comprimento(S,C2),
									  	 ((C1 \= 0; C2 \= 0), retract(prestador_perfeito(ID))).

removePerfeito(cuidado(D,H,_,IDp,_,_)) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (cuidado_perfeito(D,H,IDp), cuidado(D,H,IDu,IDp,Ds,C)), S),
										  solucoes(-cuidado(D,H,IDu,IDp,Ds,C), (cuidado_perfeito(D,H,IDp), -cuidado(D,H,IDu,IDp,Ds,C)), R),
									  	  removeLista(S),
									  	  removeLista(R),
									  	  comprimento(S,C1),
									  	  comprimento(S,C2),
									  	  ((C1 \= 0; C2 \= 0), retract(cuidado_perfeito(D,H,IDp))).

removeListaImprecisos([]).
removeListaImprecisos([utente(ID,_,_,_,_)]) :- retract(excecao(utente(ID,_,_,_,_))), (retract(utente_impreciso_idade(ID)); retract(utente_impreciso_morada(ID))).
removeListaImprecisos([utente(ID,_,_,_,_)|T]) :- retract(excecao(utente(ID,_,_,_,_))), removeListaImprecisos(T).
removeListaImprecisos([prestador(ID,_,_,_,_)]) :- retract(excecao(prestador(ID,_,_,_,_))), retract(prestador_impreciso_idInst(ID)).
removeListaImprecisos([prestador(ID,_,_,_,_)|T]) :- retract(excecao(prestador(ID,_,_,_,_))), removeListaImprecisos(T).
removeListaImprecisos([cuidado(D,H,_,IDp,_,_)]) :- retract(excecao(cuidado(D,H,_,IDp,_,_))), 
												   (retract(cuidado_impreciso_idUt(D,H,IDp)); 
												   retract(cuidado_impreciso_descricao(D,H,IDp)); 
												   retract(cuidado_impreciso_preco(D,H,IDp))).
removeListaImprecisos([cuidado(D,H,_,IDp,_,_)|T]) :- retract(excecao(cuidado(D,H,_,IDp,_,_))), removeListaImprecisos(T).

testaPerfeito(R,_) :- R \= 0.
testaPerfeito(0,utente(ID,_,_,_,_)) :- assert(utente_perfeito(ID)).
testaPerfeito(0,prestador(ID,_,_,_,_)) :- assert(prestador_perfeito(ID)).
testaPerfeito(0,cuidado(D,H,_,IDp,_,_)) :- assert(cuidado_perfeito(D,H,IDp)).

removeIncertoEspecifico(utente(ID,_,_,_,_),i) :- solucoes(utente(ID,N,I,M,D), (utente(ID,N,I,M,D), (I == xpto6; I == xpto12)), S),
									 			 removeLista(S).
removeIncertoEspecifico(utente(ID,_,_,_,_),m) :- solucoes(utente(ID,N,I,M,D), (utente(ID,N,I,M,D), (M == xpto7; I == xpto8)), S),
									 			 removeLista(S).
removeIncertoEspecifico(utente(ID,_,_,_,_),im) :- solucoes(utente(ID,N,I,M,D), (utente(ID,N,I,M,D), (I == xpto6; I == xpto12; M == xpto7)), S),
									 			 removeLista(S).
removeIncertoEspecifico(cuidado(D,H,_,IDp,_,_),idu) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (cuidado(D,H,IDu,IDp,Ds,C), 
													   (IDu == xpto11; IDu == xpto14; IDu == xpto16; IDu == xpto18)), S),
									 			 	   removeLista(S).
removeIncertoEspecifico(cuidado(D,H,_,IDp,_,_),d) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (cuidado(D,H,IDu,IDp,Ds,C), 
													 (Ds == xpto9; Ds == xpto15; Ds == xpto19; Ds == xpto21)), S),
									 			 	 removeLista(S).
removeIncertoEspecifico(cuidado(D,H,_,IDp,_,_),c) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (cuidado(D,H,IDu,IDp,Ds,C), 
													 (C == xpto10; C == xpto17; C == xpto20; C == xpto22)), S),
									 			 	 removeLista(S).
removeIncertoEspecifico(cuidado(D,H,_,IDp,_,_),idud) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (cuidado(D,H,IDu,IDp,Ds,C), 
													   (IDu == xpto11; IDu == xpto14; IDu == xpto16; IDu == xpto18; Ds == xpto9; Ds == xpto21)), S),
									 			 	   removeLista(S).
removeIncertoEspecifico(cuidado(D,H,_,IDp,_,_),iduc) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (cuidado(D,H,IDu,IDp,Ds,C), 
													    (IDu == xpto11; IDu == xpto14; IDu == xpto16; IDu == xpto18; C == xpto10; C == xpto22)), S),
									 			 	    removeLista(S).
removeIncertoEspecifico(cuidado(D,H,_,IDp,_,_),dc) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (cuidado(D,H,IDu,IDp,Ds,C), 
													  (Ds == xpto9; Ds == xpto15; Ds == xpto19; Ds == xpto21; C == xpto10; C == xpto17)), S),
									 			 	  removeLista(S).
removeIncertoEspecifico(cuidado(D,H,_,IDp,_,_),idudc) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (cuidado(D,H,IDu,IDp,Ds,C), 
													   (IDu == xpto11; IDu == xpto14; IDu == xpto16; IDu == xpto18; Ds == xpto9; Ds == xpto21; C == xpto10)), S),
									 			 	   removeLista(S).
