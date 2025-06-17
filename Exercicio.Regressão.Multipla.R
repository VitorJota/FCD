
#Fazer a matriz de correlações. Dizer quais são as 3 variáveis com maior correlação com o nível de ansiedade.

# Instalando pacotes
install.packages("corrplot") #mostra mapa de calor>>corelação
install.packages("datarium") #conjunto de dados
install.packages("esquisse") #faz grafico
install.packages("requise")

# Carregamento dos pacotes
library(corrplot)
library(datarium)
library(esquisse)
library(requise)

ws = ansiedade

matriz_correlacoes = cor(ws)
matriz_correlacoes
matriz_correlacoes[,12]

# QUESTÃO 1) Nivel de tres, sessão de terapia e horas de sono

# QUESTÃO 2) Gerar Matriz
library(ggplot2)

library(ggplot2)

ggplot(ansiedade) +
 aes(x = sessoes_terapia_mes, y = nivel_ansiedade) +
 geom_point(colour = "#112446") +
 theme_minimal()

ggplot(ws) +
 aes(x = nivel_stress, y = nivel_ansiedade) +
 geom_point(colour = "#112446") +
 theme_minimal()
esquisser(ws)

ggplot(ws) +
 aes(x = horas_de_sono, y = nivel_ansiedade) +
 geom_point(colour = "#112446") +
 theme_minimal()


# Calcula tamanho da amostra de treino
tamanho_treino = 0.8 * nrow(ws)
set.seed(123) 
treino_indices = sample(1:nrow(ws), size = tamanho_treino)

dados_treino = ws[treino_indices,]
dados_teste = ws[-treino_indices,] 

modelo = lm(data = dados_treino, formula = nivel_ansiedade ~.) 
summary(modelo)

# QUESTÃO 4) Calcular a acurácia do modelo (MAPE)

install.packages("Mlmetrics")
library(Mlmetrics)

predicoes = predict(modelo, dados_teste[,1:11])
MAPE(predicoes, dados_teste$nivel_ansiedade)
