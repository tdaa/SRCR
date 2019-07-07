#normalização dos dados
dados$volatile.acidity <- dados$volatile.acidity * 0.1
dados$residual.sugar <- dados$residual.sugar * 0.01
dados$sulphates <- dados$sulphates * 0.1
dados$alcohol <- dados$alcohol * 0.01

#dividir os dados iniciais em casos para treino
treino <- dados[1:3000, ]

#... e casos para teste
teste <- dados[3001:6497, ]

formula <- quality ~ alcohol + volatile.acidity + sulphates + residual.sugar + wine.color

rna <- neuralnet(formula,treino,hidden = c(4),lifesign = "full",linear.output = TRUE,threshold = 0.1,algorithm = "rprop+")

teste4 <- subset(teste,select = c(alcohol, volatile.acidity, sulphates, residual.sugar, wine.color))

#testar a rede com os novos casos
rna.resultados <- compute(rna,teste4)

#imprimir resultados
resultados <- data.frame(atual = teste$quality,previsao = rna.resultados$net.result)

#imprimir resultados arredondados
resultados$previsao <- round(resultados$previsao,digits = 8)

View(resultados)

#calcular o RMSE
rmse(c(teste$quality),c(resultados$previsao))