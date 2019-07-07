set.seed(1234567890)

library(neuralnet)
library(hydroGOF)
library(leaps)
library(arules)

dadosR <- read.csv("C:\\Users\\salet\\OneDrive\\Ambiente de Trabalho\\Universidade\\MIEI\\3ano\\SRCR\\trabalhoPratico3\\winequality-red.csv", header = TRUE, sep = ";", dec = ".")
dadosW <- read.csv("C:\\Users\\salet\\OneDrive\\Ambiente de Trabalho\\Universidade\\MIEI\\3ano\\SRCR\\trabalhoPratico3\\winequality-white.csv", header = TRUE, sep = ";", dec = ".")

dadosR <- cbind(wine.color = 1, dadosR)
dadosW <- cbind(wine.color = 2, dadosW)

dadosRW <- rbind(dadosR, dadosW)

dados <- dadosRW[sample(nrow(dadosRW)),]

#dividir os dados iniciais em casos para treino
treino <- dados[1:3000, ]

#... e casos para teste
teste <- dados[3001:6497, ]

#seleção de variáveis mais significativas
funcao <- quality ~ wine.color + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol

selecao <- regsubsets(funcao,dados,method = "forward")
summary(selecao)

selecao <- regsubsets(funcao,dados,method = "backward")
summary(selecao)