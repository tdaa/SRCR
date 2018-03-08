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

%Extensão do predicado instituicao: #IdInst, Designacao, Localidade -> {V,F}

instituicao(1, hospital_de_braga, braga).
instituicao(2, hospital_da_trofa, braga).
instituicao(3, hospital_sao_joao, porto).
instituicao(4, hospital_da_luz, lisboa).

%Extensão do predicado cuidado: data, hora, #IdUt, #IdPrest, Descricao, Custo -> {V,F}

cuidado(2-6-2018, 15:30, 3, 3, checkup, 10).
cuidado(10-4-2018, 16:00, 2, 1, ansiedade, 4).
cuidado(16-5-2018, 9:00, 3, 4, alergia_na_pele, 60).
cuidado(20-10-2018, 10:45, 1, 4, acne, 60).

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
-utente( ID,N,I,M ) :: (solucoes( ID, cuidado(A,H,ID,IDp,D,C), S),
						comprimento(S,L),
						L==0
					   ).


-prestador( ID,N,E,IDi ) :: (solucoes( ID, cuidado(A,H,IDu,ID,D,C), S),
								comprimento(S,L),
								L==0
					   		).


-instituicao( ID,N,M ) :: (solucoes( ID, prestador(IDp,Np,E,ID), S),
								comprimento(S,L),
								L==0
					   		).

% Identificar utentes por critérios de seleção
identificaUtenteID(ID,N) :- utente(ID,N,I,M).
identificaUtenteNome(N,S) :- solucoes((ID,N), utente(ID,N,I,M), S).
identificaUtenteIdade(I,S) :- solucoes((ID,N), utente(ID,N,I,M), S).
identificaUtenteMorada(M,S) :- solucoes((ID,N), utente(ID,N,I,M), S).

% Identificar prestadores por critérios de seleção
identificaPrestadorID(ID,N) :- prestador(ID,N,E,IDi).
identificaPrestadorNome(N,S) :- solucoes((ID,N), prestador(ID,N,E,IDi), S).
identificaPrestadorEsp(E,S) :- solucoes((ID,N), prestador(ID,N,E,IDi), S).
identificaPrestadorIDinst(IDi,S) :- solucoes((ID,N), prestador(ID,N,E,IDi), S).

% Identificar as instituições prestadoras de cuidados de saúde por critérios de seleção
identificaInstID(ID,N) :- instituicao(ID,N,L).
identificaInstNome(N,S) :- solucoes((ID,N,L), instituicao(ID,N,L), S).
identificaInstLoc(L,S) :- solucoes((ID,N), instituicao(ID,N,L), S).

% Identificar cuidados de saúde prestados por critérios de seleção tais como instituição/cidade/datas
identificaCuidadoIDInst(IDi,S) :- solucoes((D,H,IDu,IDp,DC,C), (prestador(IDp,_,_,IDi), cuidado(D,H,IDu,IDp,DC,C)), S).
identificaCuidadoNomeInst(Ni,S) :- solucoes((D,H,IDu,IDp,DC,C), (instituicao(IDi,Ni,_), prestador(IDp,_,_,IDi), cuidado(D,H,IDu,IDp,DC,C)), S).
identificaCuidadoCidade(L,S) :- solucoes((D,H,IDu,IDp,DC,C), (instituicao(IDi,_,L), prestador(IDp,_,_,IDi), cuidado(D,H,IDu,IDp,DC,C)), S).
identificaCuidadoData(D,S) :- solucoes((D,H,IDu,IDp,DC,C), cuidado(D,H,IDu,IDp,DC,C), S).
identificaCuidadoDataHora(D,H,S) :- solucoes((D,H,IDu,IDp,DC,C), cuidado(D,H,IDu,IDp,DC,C), S).

%Calcular o custo total dos cuidados de saúde por critérios tais como utente/especialidade/prestador/datas
