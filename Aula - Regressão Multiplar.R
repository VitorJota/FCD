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
# Carregando dados
dados<-marketing

# Analise grafica
esquisser (dados)

# Separação em treino e teste
#Ideia: Contruir modelo e testar, porém não tem dados novos
# separa x e y
# pegar pedaço de x e y
# seta de tribuição ou igual

# Calcula tamanho da amostra de treino
tamanho_treino = 0.8 * nrow(dados)

# Semente aleatória
set.seed(123) 

# Sorteio de dados de treino
treino_indices = sample(1:nrow(dados), size = tamanho_treino)

dados_treino = dados[treino_indices,]#formação matricial, pego todas as colunas desse intervalo
dados_teste = dados[-treino_indices,] #traço: no treino pega todos que não estão em treino_indices, ou seja os que sobrou

# Matriz de correlaçoes : correlação de person
matriz_correlacoes = cor(dados)
matriz_correlacoes
matriz_correlacoes[,4]
# Mapa de calor
corrplot(matriz_correlacoes, method = "number") # Mais proximo do azul mais correlação

# Modelo de regressão

modelo = lm(data = dados_treino, formula = sales ~.) # vendas em função de todas as outras variaveis
summary(modelo)
  # resultado: Adjusted R-squared:  0.888 (poder explicativo)
  # quanto menor o P - Value melho, quanto maior menor corelação
modelo2 = lm(data = dados_treino, formula = sales ~ youtube + facebook)
summary(modelo2)

# Acuracia do modelo

install.packages("Mlmetrics")
library(Mlmetrics)

# Predições na amostra de tete
predicoes = predict(modelo2, dados_teste[,1:3])

MAPE(predicoes, dados_teste$sales)





