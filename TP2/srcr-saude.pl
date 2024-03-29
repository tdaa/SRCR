%Sistemas de Representação de Conhecimento e Raciocínio
%Exercício 1 - Prestação de Cuidados de Saúde

:- set_prolog_flag( discontiguous_warnings, off).
:- set_prolog_flag( single_var_warnings, off).
:- set_prolog_flag( unknown, fail).

%Definições iniciais

:- op(900, xfy, '::').
:- op(995, xfy, '&&').
:- op(996, xfy, '##').
:- op(997, xfy, '~').
:- op(998, xfy, '^').
:- op(999, xfy, '*').
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
excecao(cuidado(2018-12-8, 7:00, 10, 8, dores_de_cabeca, 35)).
excecao(cuidado(2018-12-8, 7:00, 10, 8, dores_de_cabeca, 36)).
excecao(cuidado(2018-12-8, 7:00, 10, 8, dores_de_cabeca, 37)).
cuidado_impreciso_preco(2018-12-8, 7:00, 8).
excecao(cuidado(2018-12-21, 21:00, 11, 12, checkup, 5)).
excecao(cuidado(2018-12-21, 21:00, 11, 12, febre, 5)).
cuidado_impreciso_descricao(2018-12-21, 21:00, 12).
excecao(cuidado(2018-12-28, 15:00, 9, 7, ataque_cardiaco, 38)).
excecao(cuidado(2018-12-28, 15:00, 13, 7, ataque_cardiaco, 38)).
cuidado_impreciso_idUt(2018-12-28, 15:00, 7).
-cuidado(2019-1-5, 17:45, 3, 5, esquizofrenia, 10).
cuidado_perfeito(2019-1-5, 17:45, 5).
-cuidado(2019-1-5, 17:45, 6, 8, epilepsia, 60).
cuidado_perfeito(2019-1-5, 17:45, 8).

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
excecao(utente(Id,N,I,M,A)) :- utente(Id,N,xpto1,M,A).

%Invariante que não permite a adição de conhecimento no caso de exister o respetivo termo com nulo interdito

+utente(Id,N,I,M,A) :: (solucoes(Is,(utente(Id,_,Is,_,_), nulo_utente_idade(Is)),S),
						comprimento(S,L),
						L==0).

nulo_utente_morada(xpto2).
excecao(utente(Id,N,I,M,A)) :- utente(Id,N,I,xpto2,A).

%Invariante que não permite a adição de conhecimento no caso de exister o respetivo termo com nulo interdito

+utente(Id,N,I,M,A) :: (solucoes(Ms,(utente(Id,_,_,Ms,_), nulo_utente_morada(Ms)),S),
						comprimento(S,L),
						L==0).

%Nulos interditos sobre o identificador da intituição dos prestadores e os respetivos Invariantes
nulo_prestador_idInst(xpto3).
excecao(prestador(Id,N,E,Idi,A)) :- prestador(Id,N,E,xpto3,A).

%Invariante que não permite a adição de conhecimento no caso de exister o respetivo termo com nulo interdito

+prestador(Id,N,E,Idi,A) :: (solucoes(Ids,(prestador(Id,_,_,Ids,_), nulo_prestador_idInst(Ids)),S),
							 comprimento(S,L),
							 L==0).

%Nulos interditos sobre a descricao e o custo dos cuidados e os respetivos Invariantese
nulo_cuidado_descricao(xpto4).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,xpto4,C).

%Invariante que não permite a adição de conhecimento no caso de exister o respetivo termo com nulo interdito

+cuidado(D,H,IDu,IDp,Ds,C) :: (solucoes(Dss,(cuidado(D,H,IDu,IDp,Dss,C), nulo_cuidado_descricao(Dss)),S),
							   comprimento(S,L),
							   L==0).

nulo_cuidado_custo(xpto5).
excecao(cuidado(D,H,IDu,IDp,Ds,C)) :- cuidado(D,H,IDu,IDp,Ds,xpto5).

%Invariante que não permite a adição de conhecimento no caso de exister o respetivo termo com nulo interdito

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

%Extensão do predicado evolucao que só é utilizado para a instituicao

evolucao(Termo) :- solucoes(Inv, +Termo :: Inv, S),
				   insere(Termo), 
				   teste(S).

%Invariante Estrutural: não permitir a inserção de conhecimento repetido

+instituicao(ID,D,C)  :: (solucoes(ID, instituicao(ID,X,Y), S),
					      comprimento(S,L),
					      L =< 1).

%Invariante Referencial: verificar se utente e prestador existem na base de conhecimento, caso contrário não é permitida a adição do cuidado

+cuidado(D,H,IDu,IDp,DC,C) :: (solucoes(IDu, (utente(IDu,_,_,_,_); (excecao(utente(IDu,_,I,M,_)), I \= xpto6, I \= xpto12, M \= xpto7)), S),
							   comprimento(S,L),
							   L==1).

+cuidado(D,H,IDu,IDp,DC,C) :: (solucoes(IDp, (prestador(IDp,_,_,_,_); (excecao(prestador(IDp,_,_,Idi,_)), Idi \= 8)), S),
							   comprimento(S,L),
							   L==1).

%Invariante Referencial: verificar se a instituição existe na base de conhecimento, caso contrário não é permitida a adição do prestador

+prestador(ID,N,E,IDi,D) :: (solucoes(IDp, instituicao(IDi,_,_), S),
							 comprimento(S,L),
							 L==1).

%Invariante Referencial: verificar se já existe conhecimento positivo sobre o utente na base de conhecimento e, 
%em caso afirmativo, se esta já foi inserida à mais de 1 ano, caso contrário não é permitida a adição do utente

+utente(ID,N,I,M,D) :: (solucoes(ID, (utente(ID,_,_,_,Ds), utente_perfeito(ID), abaixo1ano(D,Ds)),S),
						comprimento(S,L),
						L=<1).

%Invariante Referencial: verificar se não existe conhecimento perfeito negativo sobre o utente na base de conhecimento, 
%caso contrário não é permitida a adição do utente

+utente(ID,N,I,M,D) :: (solucoes(ID, (utente_perfeito(ID), -utente(ID,_,_,_,_)), S),
						comprimento(S,L),
						L==0).

%Invariante Referencial: verificar se existe conhecimento imperfeito sobre o utente na base de conhecimento e, em caso afirmativo,
%se a data de inserção do mesmo é inferior ao do utente a inserir, caso contrário não é permitida a adição do utente

+utente(ID,N,I,M,D) :: (solucoes(ID, ((utente(ID,_,_,_,Ds); excecao(utente(ID,_,_,_,Ds))), D < Ds), S),
						comprimento(S,L),
						L==0).

%Invariante Referencial: verificar se existe conhecimento negativo associado a conhecimento incerto sobre o utente na base de conhecimento e, 
%em caso afirmativo, verifica se o utente a adicionar não entra em contradição com o conhecimento negativo presente à priori,
%caso contrário não é permitida a adição do utente

+utente(ID,N,I,M,D) :: (solucoes(ID, (utente(ID,N,Is,Ms,D), 
						((Is == xpto6, -utente(ID,N,In,Ms,D), I == In); 
						(Ms == xpto7, -utente(ID,N,Is,Mn,D), M == Mn);
						(Is == xpto12, -utente(ID,N,In,Mn,D), (I == In; M == Mn)))), S),
						comprimento(S,L),
						L==0).

%Invariante Referencial: verificar se já existe conhecimento positivo sobre o prestador na base de conhecimento e,
%em caso afirmativo, se esta já foi inserida à mais de 4 anos, caso contrário não é permitida a adição do prestador

+prestador(ID,N,E,IDi,D) :: (solucoes(ID, (prestador(ID,_,_,_,Ds), prestador_perfeito(ID), abaixo4anos(D,Ds)),S),
							 comprimento(S,L),
							 L=<1).

%Invariante Referencial: verificar se não existe conhecimento perfeito negativo sobre o prestador na base de conhecimento, 
%caso contrário não  é permitida a adição do prestador

+prestador(ID,N,E,IDi,D) :: (solucoes(ID, (prestador_perfeito(ID), -prestador(ID,_,_,_,_)), S),
							 comprimento(S,L),
							 L==0).

%Invariante Referencial: verificar se existe conhecimento imperfeito sobre o prestador na base de conhecimento e, em caso afirmativo,
%se a data de inserção do mesmo é inferior ao do prestador a inserir, caso contrário não é permitida a adição do prestador

+prestador(ID,N,I,M,D) :: (solucoes(ID, ((prestador(ID,_,_,_,Ds); excecao(prestador(ID,_,_,_,Ds))), D < Ds), S),
						   comprimento(S,L),
						   L==0).

%Invariante Referencial: verificar se existe conhecimento negativo associado a conhecimento incerto sobre o prestador na base de conhecimento e, 
%em caso afirmativo, verifica se o prestador a adicionar não entra em contradição com o conhecimento negativo presente à priori,
%caso contrário não é permitida a adição do prestador

+prestador(ID,N,E,IDi,D) :: (solucoes(ID, (prestador(ID,N,E,IDs,D), IDs == xpto8, -prestador(ID,N,E,IDn,D), IDi == IDn), S),
							 comprimento(S,L),
							 L==0).

%Invariante Referencial: verificar se não existe conhecimento positivo sobre o cuidado na base de conhecimento, 
%caso contrário não é permitida a adição do cuidado

+cuidado(D,H,IDu,IDp,Ds,C) :: (solucoes((D,H,IDp), (cuidado(D,H,_,IDp,_,_), cuidado_perfeito(D,H,IDp)),S),
							   comprimento(S,L),
							   L=<1).

%Invariante Referencial: verificar se não existe conhecimento perfeito negativo sobre o cuidado na base de conhecimento, 
%caso contrário não é permitida a adição do cuidado

+cuidado(D,H,IDu,IDp,Ds,C) :: (solucoes(ID, (cuidado_perfeito(D,H,IDp), -cuidado(D,H,_,IDp,_,_)), S),
							  comprimento(S,L),
							  L==0).

%Invariante Referencial: verificar se existe conhecimento negativo associado a conhecimento incerto sobre o cuidado na base de conhecimento e, 
%em caso afirmativo, verifica se o cuidado a adicionar não entra em contradição com o conhecimento negativo presente à priori,
%caso contrário não é permitida a adição do cuidado

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

%Extensão do predicado evolucaoPositiva que permite a adição de conhecimento positivo

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

%Invariante Referencial: verificar se não existe conhecimento positivo associado a determinado utente, 
%caso contrário não é permitida a adição do conhecimento negativo do utente

+(-utente(ID,N,I,M,D)) :: (solucoes(ID, (utente(ID,_,Is,Ms,_), Is \= xpto6, Is \= xpto12, Ms \= xpto7), S),
						   comprimento(S,L),
						   L==0).

%Invariante Referencial: verificar se não existe conhecimento impreciso associado a determinado utente, 
%caso contrário não é permitida a adição do conhecimento negativo do utente

+(-utente(ID,N,I,M,D)) :: (solucoes(ID, (utente_impreciso_idade(ID); utente_impreciso_morada(ID)), S),
						   comprimento(S,L),
						   L==0).

%Invariante <Estrutural: verificar se não existe conhecimento negativo repetido na base de conhecimento

+(-utente(ID,N,I,M,D)) :: (solucoes(ID, (-utente(ID,N,I,M,D)), S),
						   comprimento(S,L),
						   L=<2).

%Invariantes Referenciais: verificar se não existe conhecimento interdito sobre a idade ou morada do utente, caso contrário não é permitida a adição do utente

+(-utente(ID,N,I,M,D)) :: (solucoes(Is,(utente(ID,_,Is,_,_), nulo_utente_idade(Is)),S),
						   comprimento(S,L),
						   L==0).

+(-utente(ID,N,I,M,D)) :: (solucoes(Ms,(utente(ID,_,_,Ms,_), nulo_utente_morada(Ms)),S),
						   comprimento(S,L),
						   L==0).

%Invariante Referencial: verificar se não existe conhecimento positivo associado a determinado prestador, 
%caso contrário não é permitida a adição do conhecimento negativo do prestador

+(-prestador(ID,N,E,IDi,D)) :: (solucoes(ID, (prestador(ID,_,_,IDs,_), IDs \= xpto8), S),
						   		comprimento(S,L),
						   		L==0).

%Invariante Referencial: verificar se não existe conhecimento impreciso associado a determinado prestador, 
%caso contrário não é permitida a adição do conhecimento negativo do prestador

+(-prestador(ID,N,E,IDi,D)) :: (solucoes(ID, prestador_impreciso_idInst(ID), S),
						   		comprimento(S,L),
						  		L==0).

%Invariante Estrutural: verificar se não existe conhecimento negativo repetido na base de conhecimento

+(-prestador(ID,N,E,IDi,D)) :: (solucoes(ID, -prestador(ID,N,E,IDi,D), S),
						   		comprimento(S,L),
						   		L=<2).

%Invariante Referencial: verificar se não existe conhecimento interdito sobre o ID da instituição do prestador, caso contrário não é permitida a adição do prestador

+(-prestador(ID,N,E,IDi,D)) :: (solucoes(IDs,(prestador(ID,_,_,IDs,_), nulo_prestador_idInst(IDs)),S),
						   		comprimento(S,L),
						   		L==0).

%Invariante Referencial: verificar se não existe conhecimento positivo associado a determinado cuidado, 
%caso contrário não é permitida a adição do conhecimento negativo do cuidado

+(-cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes((D,H,IDp), (cuidado(D,H,IDus,IDp,Dss,Cs), IDus \= xpto11, IDus \= xpto14, IDus \= xpto16, IDus \= xpto18, Dss \= xpto9, Dss \= xpto21, Cs \= xpto10), S),
						   		  comprimento(S,L),
						   		  L==0).

%Invariante Referencial: verificar se não existe conhecimento impreciso associado a determinado cuidado, 
%caso contrário não é permitida a adição do conhecimento negativo do cuidado

+(-cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes((D,H,IDp), (cuidado_impreciso_descricao(D,H,IDp); cuidado_impreciso_preco(D,H,IDp); cuidado_impreciso_idUt(D,H,IDp)), S),
						   		  comprimento(S,L),
						  		  L==0).

%Invariante Estrutural: verificar se não existe conhecimento negativo repetido na base de conhecimento

+(-cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes((D,H,IDp), -cuidado(D,H,IDu,IDp,Ds,C), S),
						   		  comprimento(S,L),
						   		  L=<2).

%Invariantes Referenciais: verificar se não existe conhecimento interdito sobre a descrição ou custo do cuidado, caso contrário não é permitida a adição do cuidado

+(-cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes((D,H,IDp),(cuidado(D,H,IDus,IDp,Ds,Cs), nulo_cuidado_custo(Cs)),S),
						   		  comprimento(S,L),
						   		  L==0).

+(-cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes((D,H,IDp),(cuidado(D,H,IDus,IDp,Ds,Cs), nulo_cuidado_descricao(Ds)),S),
						   		  comprimento(S,L),
						   		  L==0).

%Extensão do predicado evolucaoNegativa que permite a adição de conhecimento negativo

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
										 	   solucoes(ID, (cuidado(D,H,IDu,IDp,Ds,C), IDu == xpto11; IDu == xpto14; IDu == xpto14; IDu == xpto16; IDu == xpto18; Ds == xpto9; Ds == xpto21; C == xpto10), R),
										 	   testaPerfeito(R,cuidado(D,H,_,IDp,_,_)).

%Invariante que impossibilita a adição de conhecimento impreciso quando já existe conhecimento positivo na base de conhecimento

~(utente(ID,N,I,M,D)) :: (solucoes(ID, (utente(ID,_,Is,Ms,_), Is \= xpto6, Is \= xpto12, Ms \= xpto7), S),
						  comprimento(S,L),
						  L==0).

%Invariante que impossibilita a adição de conhecimento impreciso quando já existe conhecimento perfeito negativo na base de conhecimento

~(utente(ID,N,I,M,D)) :: (solucoes(ID, (-utente(ID,_,_,_,_), utente_perfeito(ID)), S),
						  comprimento(S,L),
						  L==0).

%Invariantes que impossibilitam a adição de conhecimento impreciso quando já existe conhecimento interdito na base de conhecimento

~(utente(ID,N,I,M,D)) :: (solucoes(ID, (utente(ID,_,Is,_,_), nulo_utente_idade(Is)), S),
						  comprimento(S,L),
						  L==0).

~(utente(ID,N,I,M,D)) :: (solucoes(ID, (utente(ID,_,_,Ms,_), nulo_utente_morada(Ms)), S),
						  comprimento(S,L),
						  L==0).

%Invariante que impossibilita a adição de conhecimento impreciso repetido

~(utente(ID,N,I,M,D)) :: (solucoes(ID, (excecao(utente(ID,N,I,M,Ds)), utente_impreciso_idade(ID)), S),
						  comprimento(S,L),
						  L=<1).

~(utente(ID,N,I,M,D)) :: (solucoes(ID, (excecao(utente(ID,N,I,M,Ds)), utente_impreciso_morada(ID)), S),
						  comprimento(S,L),
						  L=<1).

%Invariante que impossibilita a adição de conhecimento impreciso quando já existe conhecimento positivo na base de conhecimento

~(prestador(ID,N,E,IDi,D)) :: (solucoes(ID, (prestador(ID,_,_,IDs,_), IDs \= xpto8), S),
							   comprimento(S,L),
							   L==0).

%Invariante que impossibilita a adição de conhecimento impreciso quando já existe conhecimento perfeito negativo na base de conhecimento

~(prestador(ID,N,E,IDi,D)) :: (solucoes(ID, (-prestador(ID,_,_,_,_), prestador_perfeito(ID)), S),
							   comprimento(S,L),
							   L==0).

%Invariante que impossibilita a adição de conhecimento impreciso quando já existe conhecimento interdito na base de conhecimento

~(prestador(ID,N,E,IDi,D)) :: (solucoes(ID, (prestador(ID,_,_,IDs,_), nulo_prestador_idInst(IDs)), S),
							   comprimento(S,L),
							   L==0).

%Invariante que impossibilita a adição de conhecimento impreciso repetido

~(prestador(ID,N,E,IDi,D)) :: (solucoes(ID, (excecao(prestador(ID,N,E,IDi,Ds)), prestador_impreciso_idInst(ID)), S),
							   comprimento(S,L),
							   L=<1).

%Invariante que impossibilita a adição de conhecimento impreciso quando já existe conhecimento positivo na base de conhecimento

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,IDus,IDp,Dss,Cs), 
								 IDus \= xpto11, IDus \= xpto14, IDus \= xpto16, IDus \= xpto18, Dss \= xpto9, Dss \= xpto21, Cs \= xpto10), S),
							     comprimento(S,L),
							     L==0).

%Invariante que impossibilita a adição de conhecimento impreciso quando já existe conhecimento perfeito negativo na base de conhecimento

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (-cuidado(D,H,_,IDp,_,_), cuidado_perfeito(D,H,IDp)), S),
							     comprimento(S,L),
							     L==0).

%Invariantes que impossibilitam a adição de conhecimento impreciso quando já existe conhecimento interdito na base de conhecimento

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,_,IDp,Dss,_), nulo_cuidado_descricao(Dss)), S),
							     comprimento(S,L),
							     L==0).

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,_,IDp,_,Cs), nulo_cuidado_custo(Cs)), S),
							     comprimento(S,L),
							     L==0).

%Invariante que impossibilita a adição de conhecimento impreciso repetido

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (excecao(cuidado(D,H,IDu,IDp,Ds,C)), cuidado_impreciso_descricao(ID)), S),
							     comprimento(S,L),
							     L=<1).

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (excecao(cuidado(D,H,IDu,IDp,Ds,C)), cuidado_impreciso_preco(ID)), S),
							     comprimento(S,L),
							     L=<1).

~(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (excecao(cuidado(D,H,IDu,IDp,Ds,C)), cuidado_impreciso_idUt(ID)), S),
							     comprimento(S,L),
							     L=<1).

%Extensão do predicado evolucaoImprecisa que permite a adição de conhecimento impreciso

evolucaoImprecisa(utente(ID,N,I,M,D),T) :- solucoes(Inv, ~(utente(ID,N,I,M,D)) :: Inv, S),
										   insere(excecao(utente(ID,N,I,M,D))),
										   teste(S),
										   removeIncerto(utente(ID,_,_,_,_)),
										   ((nao(T==i),nao(T==im)); utente_impreciso_idade(ID); assert(utente_impreciso_idade(ID))),
										   ((nao(T==m),nao(T==im)); utente_impreciso_morada(ID); assert(utente_impreciso_morada(ID))).

evolucaoImprecisa(prestador(ID,N,E,IDi,D)) :- solucoes(Inv, ~(prestador(ID,N,E,IDi,D)) :: Inv, S),
										   	  insere(excecao(prestador(ID,N,E,IDi,D))),
										      teste(S),
										      removeIncerto(prestador(ID,_,_,_,_)),
										      (prestador_impreciso_idInst(ID); assert(prestador_impreciso_idInst(ID))).

evolucaoImprecisa(cuidado(D,H,IDu,IDp,Ds,C),T) :- solucoes(Inv, ~(cuidado(D,H,IDu,IDp,Ds,C)) :: Inv, S),
										   	  	  insere(excecao(cuidado(D,H,IDu,IDp,Ds,C))),
										      	  teste(S),
										      	  removeIncerto(cuidado(D,H,_,IDp,_,_)),
										      	  ((nao(T==idu), nao(T==idud), nao(T==iduc), nao(T==idudc)); cuidado_impreciso_idUt(D,H,IDp); assert(cuidado_impreciso_idUt(D,H,IDp))),
										      	  ((nao(T==d), nao(T==idud), nao(T==dc), nao(T==idudc)); cuidado_impreciso_descricao(D,H,IDp); assert(cuidado_impreciso_descricao(D,H,IDp))),
										      	  ((nao(T==c), nao(T==iduc), nao(T==dc), nao(T==idudc)); cuidado_impreciso_idUt(D,H,IDp); assert(cuidado_impreciso_idUt(D,H,IDp))).

%Invariantes que garante a continuação do conhecimento interdito, isto é, um termo que tinha um argumento interdito à priori, tem que continuar com esse argumento interdito

^(utente(ID,N,I,M,D)) :: (solucoes(Is,(utente(ID,_,Is,_,_), nulo_utente_idade(Is), I\=xpto1),S),
						  comprimento(S,L),
						  L==0).

^(utente(ID,N,I,M,D)) :: (solucoes(Ms,(utente(ID,_,_,Ms,_), nulo_utente_morada(Ms), M\=xpto2),S),
						  comprimento(S,L),
						  L==0).

%Invariante que não permite a adição de conhecimento repetido na base de conhecimento

^(utente(ID,N,I,M,D)) :: (solucoes(ID,utente(ID,N,I,M,Ds),S),
						  comprimento(S,L),
						  L=<1).

%Invariantes que garante a continuação do conhecimento interdito, isto é, um termo que tinha um argumento interdito à priori, tem que continuar com esse argumento interdito

^(prestador(ID,N,E,IDi,D)) :: (solucoes(IDs,(prestador(ID,_,_,IDs,_), nulo_prestador_idInst(IDs), IDi\=xpto3),S),
						   	   comprimento(S,L),
						   	   L==0).

%Invariante que não permite a adição de conhecimento repetido na base de conhecimento

^(prestador(ID,N,E,IDi,D)) :: (solucoes(ID,prestador(ID,N,E,IDi,D),S),
						  	   comprimento(S,L),
						  	   L=<1).

%Invariantes que garante a continuação do conhecimento interdito, isto é, um termo que tinha um argumento interdito à priori, tem que continuar com esse argumento interdito

^(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,_,IDp,Dss,_), nulo_cuidado_descricao(Dss), Ds\=xpto4), S),
							     comprimento(S,L),
							     L==0).

^(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado(D,H,_,IDp,_,Cs), nulo_cuidado_custo(Cs), C\=xpto5), S),
							     comprimento(S,L),
							     L==0).

%Invariante que não permite a adição de conhecimento repetido na base de conhecimento

^(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes((D,H,IDp),cuidado(D,H,IDu,IDp,Ds,C),S),
						  	     comprimento(S,L),
						  	     L=<1).

%Extensão do predicado evolucaoInterdita que permite a adição de conhecimento interdito

evolucaoInterdita(utente(ID,N,nulo,M,D)) :- M \= nulo,
											solucoes(Inv, ^(utente(ID,N,xpto1,M,D)) :: Inv, S),
										 	insere(utente(ID,N,xpto1,M,D)),
										 	teste(S),
										 	removeImpreciso(utente(ID,_,_,_,_)),
										 	removeIncerto(utente(ID,_,_,_,_)),
										 	removePerfeito(utente(ID,_,_,_,_)).

evolucaoInterdita(utente(ID,N,I,nulo,D)) :- I \= nulo,
											solucoes(Inv, ^(utente(ID,N,I,xpto2,D)) :: Inv, S),
										 	insere(utente(ID,N,I,xpto2,D)),
										 	teste(S),
										 	removeImpreciso(utente(ID,_,_,_,_)),
										 	removeIncerto(utente(ID,_,_,_,_)),
										 	removePerfeito(utente(ID,_,_,_,_)).

evolucaoInterdita(utente(ID,N,nulo,nulo,D)) :- solucoes(Inv, ^(utente(ID,N,xpto1,xpto2,D)) :: Inv, S),
										 	   insere(utente(ID,N,xpto1,xpto2,D)),
										 	   teste(S),
										 	   removeInterdito(utente(ID,_,_,_,_)),
										 	   removeImpreciso(utente(ID,_,_,_,_)),
										       removeIncerto(utente(ID,_,_,_,_)),
										 	   removePerfeito(utente(ID,_,_,_,_)).

evolucaoInterdita(prestador(ID,N,E,nulo,D)) :- solucoes(Inv, ^(prestador(ID,N,E,xpto3,D)) :: Inv, S),
											   insere(prestador(ID,N,E,xpto3,D)),
											   teste(S),
											   removeImpreciso(prestador(ID,_,_,_,_)),
										  	   removeIncerto(prestador(ID,_,_,_,_)),
											   removePerfeito(prestador(ID,_,_,_,_)).

evolucaoInterdita(cuidado(D,H,IDu,IDp,nulo,C)) :- C \= nulo,
												  solucoes(Inv, ^(cuidado(D,H,IDu,IDp,xpto4,C)) :: Inv, S),
											   	  insere(cuidado(D,H,IDu,IDp,xpto4,C)),
											   	  teste(S),
											      removeImpreciso(cuidado(D,H,_,IDp,_,_)),
										  	      removeIncerto(cuidado(D,H,_,IDp,_,_)),
											      removePerfeito(cuidado(D,H,_,IDp,_,_)).

evolucaoInterdita(cuidado(D,H,IDu,IDp,Ds,nulo)) :- Ds \= nulo,
												   solucoes(Inv, ^(cuidado(D,H,IDu,IDp,Ds,xpto5)) :: Inv, S),
											   	   insere(cuidado(D,H,IDu,IDp,Ds,xpto5)),
											   	   teste(S),
											       removeImpreciso(cuidado(D,H,_,IDp,_,_)),
										  	       removeIncerto(cuidado(D,H,_,IDp,_,_)),
											       removePerfeito(cuidado(D,H,_,IDp,_,_)).

evolucaoInterdita(cuidado(D,H,IDu,IDp,nulo,nulo)) :- solucoes(Inv, ^(cuidado(D,H,IDu,IDp,xpto4,xpto5)) :: Inv, S),
											   	     insere(cuidado(D,H,IDu,IDp,xpto4,xpto5)),
											   	     teste(S),
											   	     removeInterdito(cuidado(D,H,_,IDp,_,_)),
											         removeImpreciso(cuidado(D,H,_,IDp,_,_)),
										  	         removeIncerto(cuidado(D,H,_,IDp,_,_)),
											         removePerfeito(cuidado(D,H,_,IDp,_,_)).

%Invariante que impossibilita a adição de conhecimento incerto no caso de já existir conhecimento positivo ou incerto sobre esse termo na base de conhecimento

*(utente(ID,N,I,M,D)) :: (solucoes(ID, utente(ID,_,_,_,_), S),
						  comprimento(S,L),
						  L==1).

%Invariante que impossibilita a adição de conhecimento incerto no caso de já existir conhecimento negativo sobre esse termo na base de conhecimento

*(utente(ID,N,I,M,D)) :: (solucoes(ID, -utente(ID,_,_,_,_), S),
						  comprimento(S,L),
						  L==0).

%Invariante que impossibilita a adição de conhecimento incerto no caso de já existir conhecimento impreciso sobre esse termo na base de conhecimento

*(utente(ID,N,I,M,D)) :: (solucoes(ID, (utente_impreciso_idade(ID); utente_impreciso_morada(ID)), S),
						  comprimento(S,L),
						  L==0).

%Invariante que impossibilita a adição de conhecimento incerto no caso de já existir conhecimento positivo ou incerto sobre esse termo na base de conhecimento

*(prestador(ID,N,E,IDi,D)) :: (solucoes(ID, prestador(ID,_,_,_,_), S),
						  	   comprimento(S,L),
						 	   L==1).

%Invariante que impossibilita a adição de conhecimento incerto no caso de já existir conhecimento negativo sobre esse termo na base de conhecimento

*(prestador(ID,N,E,IDi,D)) :: (solucoes(ID, -prestador(ID,_,_,_,_), S),
						  	   comprimento(S,L),
						  	   L==0).

%Invariante que impossibilita a adição de conhecimento incerto no caso de já existir conhecimento impreciso sobre esse termo na base de conhecimento

*(prestador(ID,N,E,IDi,D)) :: (solucoes(ID, prestador_impreciso_idInst(ID), S),
						  	   comprimento(S,L),
						  	   L==0).

%Invariante que impossibilita a adição de conhecimento incerto no caso de já existir conhecimento positivo ou incerto sobre esse termo na base de conhecimento

*(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, cuidado(D,H,_,IDp,_,_), S),
						  	     comprimento(S,L),
						 	     L==1).

%Invariante que impossibilita a adição de conhecimento incerto no caso de já existir conhecimento negativo sobre esse termo na base de conhecimento

*(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, -cuidado(D,H,_,IDp,_,_), S),
						  	     comprimento(S,L),
						  	     L==0).

%Invariante que impossibilita a adição de conhecimento incerto no caso de já existir conhecimento impreciso sobre esse termo na base de conhecimento

*(cuidado(D,H,IDu,IDp,Ds,C)) :: (solucoes(ID, (cuidado_impreciso_idUt(D,H,IDp); cuidado_impreciso_descricao(D,H,IDp); cuidado_impreciso_preco(D,H,IDp)), S),
						  	     comprimento(S,L),
						  	     L==0).

%Extensão do predicado evolucaoIncerta que permite a adição de conhecimento incerto

evolucaoIncerta(utente(ID,N,nulo,M,D)) :- solucoes(Inv, *(utente(ID,N,xpto6,M,D)) :: Inv, S),
										  insere(utente(ID,N,xpto6,M,D)),
										  teste(S).

evolucaoIncerta(utente(ID,N,I,nulo,D)) :- solucoes(Inv, *(utente(ID,N,I,xpto7,D)) :: Inv, S),
										  insere(utente(ID,N,I,xpto7,D)),
										  teste(S).

evolucaoIncerta(utente(ID,N,nulo,nulo,D)) :- solucoes(Inv, *(utente(ID,N,xpto12,xpto13,D)) :: Inv, S),
										 	 insere(utente(ID,N,xpto12,xpto13,D)),
										 	 teste(S).

evolucaoIncerta(prestador(ID,N,E,nulo,D)) :- solucoes(Inv, *(prestador(ID,N,E,xpto8,D)) :: Inv, S),
										  	 insere(prestador(ID,N,E,xpto8,D)),
										  	 teste(S).

evolucaoIncerta(cuidado(D,H,nulo,IDp,Ds,C)) :- solucoes(Inv, *(cuidado(D,H,xpto11,IDp,Ds,C)) :: Inv, S),
										  	   insere(cuidado(D,H,xpto11,IDp,Ds,C)),
										       teste(S).

evolucaoIncerta(cuidado(D,H,IDu,IDp,nulo,C)) :- solucoes(Inv, *(cuidado(D,H,IDu,IDp,xpto9,C)) :: Inv, S),
										  	    insere(cuidado(D,H,IDu,IDp,xpto9,C)),
										        teste(S).

evolucaoIncerta(cuidado(D,H,IDu,IDp,Ds,nulo)) :- solucoes(Inv, *(cuidado(D,H,IDu,IDp,Ds,xpto10)) :: Inv, S),
										  	     insere(cuidado(D,H,IDu,IDp,Ds,xpto10)),
										         teste(S).

evolucaoIncerta(cuidado(D,H,nulo,IDp,nulo,C)) :- solucoes(Inv, *(cuidado(D,H,xpto14,IDp,xpto15,C)) :: Inv, S),
										  	     insere(cuidado(D,H,xpto14,IDp,xpto15,C)),
										         teste(S).

evolucaoIncerta(cuidado(D,H,nulo,IDp,Ds,nulo)) :- solucoes(Inv, *(cuidado(D,H,xpto16,IDp,Ds,xpto17)) :: Inv, S),
										  	      insere(cuidado(D,H,xpto16,IDp,Ds,xpto17)),
										          teste(S).

evolucaoIncerta(cuidado(D,H,nulo,IDp,nulo,nulo)) :- solucoes(Inv, *(cuidado(D,H,xpto18,IDp,xpto19,xpto20)) :: Inv, S),
										  	     	insere(cuidado(D,H,xpto18,IDp,xpto19,xpto20)),
										         	teste(S).

evolucaoIncerta(cuidado(D,H,IDu,IDp,nulo,nulo)) :- solucoes(Inv, *(cuidado(D,H,IDu,IDp,xpto21,xpto22)) :: Inv, S),
										  	       insere(cuidado(D,H,IDu,IDp,xpto21,xpto22)),
										           teste(S).

%Invariante Referencial: não permitir a remoção de conhecimento que esteja a ser utilizado

-utente(ID,N,I,M,D) :: (solucoes(ID, (cuidado(Dt,H,ID,IDp,Ds,C); -cuidado(Dt,H,ID,IDp,Ds,C)) , S),
					    comprimento(S,L),
					    L==0).

-utente(ID,N,I,M,D) :: (solucoes(ID, (excecao(cuidado(Dt,H,ID,IDp,Ds,C)), 
						Ds == xpto9; Ds == xpto21; C == xpto11) , S),
					    comprimento(S,L),
					    L==0).

-prestador(ID,N,E,IDi,D) :: (solucoes(ID, (cuidado(Dt,H,IDu,ID,Ds,C); -cuidado(Dt,H,IDu,ID,Ds,C)) , S),
					         comprimento(S,L),
					         L==0).

-prestador(ID,N,E,IDi,D) :: (solucoes(ID, (excecao(cuidado(Dt,H,IDu,ID,Ds,C)),
							 IDu == xpto11; IDu == xpto14; IDu == xpto16; IDu == xpto18; Ds == xpto9; Ds == xpto21; C == xpto11) , S),
					         comprimento(S,L),
					         L==0).

-instituicao(ID,N,M) :: (solucoes(ID, (prestador(IDp,Np,E,ID); -prestador(IDp,Np,E,ID)), S),
						 comprimento(S,L),
						 L==0).

%Extensão do predicado involucao que pode ser utilizado para todos os predicados

involucao(Termo) :- solucoes(Inv, -Termo :: Inv, S),
      			    remove(Termo),
					teste(S).

%PREDICADOS EXTRA LIGADOS ÀS EVOLUCOES

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

%Extensão do predicado abaixo1ano que verifica se a diferença entre dois anos é inferior a 1

abaixo1ano(D1,D2) :- (D1-D2) < 1.

%Extensão do predicado abaixo4anos que verifica se a diferença entre dois anos é inferior a 4

abaixo4anos(D1,D2) :- (D1-D2) < 4.

%Extensão do predicado removeLista que, dada uma lista de termos, remove cada um destes da base de conhecimento

removeLista([]).
removeLista([H|T]) :- retract(H), removeLista(T).

%Extensão do predicado removeIncerto que, dado uma entidade com determinado ID, remove todo o conhecimento incerto associado à mesma

removeIncerto(utente(ID,_,_,_,_)) :- solucoes(utente(ID,N,I,M,D), (utente(ID,N,I,M,D), (I == xpto6; I == xpto12; M == xpto7)), S),
									 solucoes(-utente(ID,N,I,M,Ds), (-utente(ID,N,I,M,Ds), nao(utente_perfeito(ID))), R),
									 removeLista(S),
									 removeLista(R).
removeIncerto(prestador(ID,_,_,_,_)) :- solucoes(prestador(ID,N,E,IDi,D), (prestador(ID,N,E,IDi,D), IDi == xpto8), S),
									 	solucoes(-prestador(ID,N,E,IDi,Ds), (-prestador(ID,N,E,IDi,Ds), nao(prestador_perfeito(ID))), R),
									 	removeLista(S),
									 	removeLista(R).
removeIncerto(cuidado(D,H,_,IDp,_,_)) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (cuidado(D,H,IDu,IDp,Ds,C), 
										 	(IDu == xpto11; IDu == xpto14; IDu == xpto16; IDu == xpto18; Ds == xpto9; Ds == xpto21; C == xpto10)), S),
									 	 solucoes(-cuidado(D,H,IDu,IDp,Ds,C), (-cuidado(D,H,IDu,IDp,Ds,C), nao(cuidado_perfeito(D,H,IDp))), R),
									 	 removeLista(S),
									 	 removeLista(R).

%Extensão do predicado removeImpreciso que, dado uma entidade com determinado ID, remove todo o conhecimento impreciso associado à mesma

removeImpreciso(utente(ID,_,_,_,_)) :- solucoes(utente(ID,N,I,M,D), (excecao(utente(ID,N,I,M,D)), I \= xpto6, I \= xpto12, M \= xpto7), S),
									   removeListaImprecisos(S).
removeImpreciso(prestador(ID,_,_,_,_)) :- solucoes(prestador(ID,N,E,IDi,D), (excecao(prestador(ID,N,E,IDi,D)), IDi \= xpto8), S),
									   	  removeListaImprecisos(S).
removeImpreciso(cuidado(D,H,_,IDp,_,_)) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (excecao(cuidado(D,H,IDu,IDp,Ds,C)), 
										   IDu \= xpto11, IDu \= xpto14, IDu \= xpto16, IDu \= xpto18, Ds \= xpto9, Ds \= xpto21, C \= xpto10), S),
									   	   removeListaImprecisos(S).

%Extensão do predicado removePerfeitoDatas que, dado uma entidade com determinado ID e uma data, remove todo o conhecimento perfeito associado à mesma
%em que data do termo encontrado seja diferente da data fornecida

removePerfeitoDatas(utente(ID,_,_,_,D)) :- solucoes(utente(ID,N,I,M,Ds), (utente_perfeito(ID), utente(ID,N,I,M,Ds), Ds \= D), S),
									  	     removeLista(S),
									  	     comprimento(S,C1),
									  	     (nao(C1 \= 0); retract(utente_perfeito(ID))).

removePerfeitoDatas(prestador(ID,_,_,_,D)) :- solucoes(prestador(ID,N,E,IDi,Ds), (prestador_perfeito(ID), prestador(ID,N,E,IDi,Ds), Ds \= D), S),
										 	  removeLista(S),
									  	 	  comprimento(S,C1),
									  	 	  (nao(C1 \= 0); retract(prestador_perfeito(ID))).

%Extensão do predicado removePerfeito que, dado uma entidade com determinado ID, remove todo o conhecimento perfeito associado à mesma

removePerfeito(utente(ID,_,_,_,_)) :- solucoes(utente(ID,N,I,M,Ds), (utente_perfeito(ID), utente(ID,N,I,M,Ds), I\=xpto1, M\=xpto2), S),
									  solucoes(-utente(ID,N,I,M,Ds), (utente_perfeito(ID), -utente(ID,N,I,M,Ds), I\=xpto1, M\=xpto2), R),
									  removeLista(S),
									  removeLista(R),
									  comprimento(S,C1),
									  comprimento(R,C2),
									  ((nao(C1 \= 0), nao(C2 \= 0)); retract(utente_perfeito(ID))).

removePerfeito(prestador(ID,_,_,_,_)) :- solucoes(prestador(ID,N,E,IDi,D), (prestador_perfeito(ID), prestador(ID,N,E,IDi,D), IDi\=xpto3), S),
										 solucoes(-prestador(ID,N,E,IDi,D), (prestador_perfeito(ID), -prestador(ID,N,E,IDi,D), IDi\=xpto3), R),
									  	 removeLista(S),
									  	 removeLista(R),
									  	 comprimento(S,C1),
									  	 comprimento(R,C2),
									  	 ((nao(C1 \= 0), nao(C2 \= 0)); retract(prestador_perfeito(ID))).

removePerfeito(cuidado(D,H,_,IDp,_,_)) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), (cuidado_perfeito(D,H,IDp), cuidado(D,H,IDu,IDp,Ds,C), Ds\=xpto4, C\=xpto5), S),
										  solucoes(-cuidado(D,H,IDu,IDp,Ds,C), (cuidado_perfeito(D,H,IDp), -cuidado(D,H,IDu,IDp,Ds,C), Ds\=xpto4, C\=xpto5), R),
									  	  removeLista(S),
									  	  removeLista(R),
									  	  comprimento(S,C1),
									  	  comprimento(R,C2),
									  	  ((nao(C1 \= 0), nao(C2 \= 0)); retract(cuidado_perfeito(D,H,IDp))).

%Extensão do predicado removeInterdito que, dado uma entidade com determinado ID, remove todo o conhecimento interdito associado à mesma

removeInterdito(utente(ID,_,_,_,_)) :- solucoes(utente(ID,N,xpto1,M,Ds), (utente(ID,N,xpto1,M,Ds), M\=xpto2), S),
									   solucoes(-utente(ID,N,I,xpto2,Ds), (utente(ID,N,I,xpto2,Ds), I\=xpto1), R),
									   removeLista(S),
									   removeLista(R).
removeInterdito(cuidado(D,H,_,IDp,_,_)) :- solucoes(cuidado(D,H,IDu,IDp,xpto4,C), (cuidado(D,H,IDu,IDp,xpto4,C), C\=xpto5), S),
										   solucoes(cuidado(D,H,IDu,IDp,Ds,xpto5), (cuidado(D,H,IDu,IDp,Ds,xpto5), Ds\=xpto4), R),
										   removeLista(S),
									  	   removeLista(R).

%Extensão do predicado removeListaImprecisos que, dada uma lista de termos, remove a respetiva exceção de conhecimento impreciso

removeListaImprecisos([]).
removeListaImprecisos([utente(ID,_,_,_,_)]) :- retract(excecao(utente(ID,_,_,_,_))), (nao(utente_impreciso_idade(ID)); retract(utente_impreciso_idade(ID))), (nao(utente_impreciso_morada(ID)); retract(utente_impreciso_morada(ID))).
removeListaImprecisos([utente(ID,_,_,_,_)|T]) :- retract(excecao(utente(ID,_,_,_,_))), removeListaImprecisos(T).
removeListaImprecisos([prestador(ID,_,_,_,_)]) :- retract(excecao(prestador(ID,_,_,_,_))), retract(prestador_impreciso_idInst(ID)).
removeListaImprecisos([prestador(ID,_,_,_,_)|T]) :- retract(excecao(prestador(ID,_,_,_,_))), removeListaImprecisos(T).
removeListaImprecisos([cuidado(D,H,_,IDp,_,_)]) :- retract(excecao(cuidado(D,H,_,IDp,_,_))), 
												   (nao(cuidado_impreciso_idUt(D,H,IDp)); retract(cuidado_impreciso_idUt(D,H,IDp))), 
												   (nao(cuidado_impreciso_descricao(D,H,IDp)); retract(cuidado_impreciso_descricao(D,H,IDp))), 
												   (nao(cuidado_impreciso_preco(D,H,IDp)); retract(cuidado_impreciso_preco(D,H,IDp))).
removeListaImprecisos([cuidado(D,H,_,IDp,_,_)|T]) :- retract(excecao(cuidado(D,H,_,IDp,_,_))), removeListaImprecisos(T).

%Extensão do predicado testaPerfeito que, dada uma lista e uma entidade com determinado ID, no caso de a lista ser vazia, 
%verifica se o termo corresponde a conhecimento perfeito e, em caso negativo, adiciona o utente_perfeito desse ID

testaPerfeito([],utente(ID,_,_,_,_)) :- (utente_perfeito(ID); assert(utente_perfeito(ID))).
testaPerfeito([],prestador(ID,_,_,_,_)) :- (prestador_perfeito(ID); assert(prestador_perfeito(ID))).
testaPerfeito([],cuidado(D,H,_,IDp,_,_)) :- (cuidado_perfeito(D,H,IDp); assert(cuidado_perfeito(D,H,IDp))).
testaPerfeito(R,_).

%PREDICADOS DEMOS E RELACIONADOS

%Extensão do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

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

%Extensão do meta-predicado demo: Questao,Resposta -> {V,F}

demo(Questao,verdadeiro) :- Questao.
demo(Questao,falso) :- -Questao.
demo(Questao,desconhecido) :- nao(Questao), nao(-Questao).

%Extensão do meta-predicado demoLista: Lista_de_Questoes,Resposta -> {V,F}

demoLista([],[]).
demoLista([Q1|T],[R1|R]) :- demo(Q1,R1), demoLista(T,R).

%Extensão do meta-predicado demoComp: Composicao_de_Questoes,Resposta -> {V,F}

demoComp(Q1 && CQ, R) :- demo(Q1,R1), demoComp(CQ,R2), conjuncao(R1,R2,R).
demoComp(Q1 ## CQ, R) :- demo(Q1,R1), demoComp(CQ,R2), disjuncao(R1,R2,R).
demoComp(Q1, R) :- demo(Q1,R).


%PREDICADOS EXTRAS

%Extensão do predicado utentesAnomalias que determina uma lista dos utentes com conhecimento imperfeito

utentesAnomalias(S) :- solucoes(utente(ID,N,I,M,D), 
					   (utente(ID,N,I,M,D), 
					   (utente_impreciso_idade(ID); utente_impreciso_morada(ID); 
					   nulo_utente_idade(ID); nulo_utente_morada(M); 
					   I == xpto6; I == xpto12; M == xpto7)), 
					   S).

%Extensão do predicado prestadoresAnomalias que determina uma lista dos prestadores com conhecimento imperfeito

prestadoresAnomalias(S) :- solucoes(prestador(ID,N,E,IDi,D), 
						   (prestador(ID,N,E,IDi,D),
						   (prestador_impreciso_idInst(ID); 
						   nulo_prestador_idInst(IDi);
						   IDi == xpto8)),
						   S).

%Extensão do predicado cuidadosAnomaliasUtente que, dado o ID de um utente, determina uma lista dos cuidados com conhecimento imperfeito associados a esse ID de utente

cuidadosAnomaliasUtente(IDu,S) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), 
								  (cuidado(D,H,IDu,IDp,Ds,C),
								  (cuidado_impreciso_descricao(D,H,IDp); cuidado_impreciso_preco(D,H,IDp);
								  nulo_cuidado_descricao(Ds); nulo_cuidado_custo(C);
								  Ds == xpto9; Ds == xpto21; C == xpto11)), 
								  S).

%Extensão do predicado cuidadosAnomaliasPrestador que, dado o ID de um prestador, determina uma lista dos cuidados com conhecimento imperfeito associados a esse ID de prestador

cuidadosAnomaliasPrestador(IDp,S) :- solucoes(cuidado(D,H,IDu,IDp,Ds,C), 
								  	 (cuidado(D,H,IDu,IDp,Ds,C),
								  	 (cuidado_impreciso_idUt(D,H,IDp); cuidado_impreciso_descricao(D,H,IDp); cuidado_impreciso_preco(D,H,IDp);
								  	 nulo_cuidado_descricao(Ds); nulo_cuidado_custo(C);
								  	 IDu == xpto11; IDu == xpto14; IDu == xpto16; IDu == xpto18; Ds == xpto9; Ds == xpto21; C == xpto11)), 
								  	 S).