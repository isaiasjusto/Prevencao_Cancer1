# Carregar os dados
dados <- read.csv("C:/Users/Horus-User/Downloads/breast+cancer+wisconsin+diagnostic/wdbc.data", header = FALSE, stringsAsFactors = FALSE)

# Definir os nomes das colunas
colnames(dados) <- c("ID", "Diagnosis", 
                     "Mean Radius", "Mean Texture", "Mean Perimeter", "Mean Area", "Mean Smoothness", "Mean Compactness", "Mean Concavity", 
                     "Mean Concave Points", "Mean Symmetry", "Mean Fractal Dimension", 
                     "SE Radius", "SE Texture", "SE Perimeter", "SE Area", "SE Smoothness", "SE Compactness", "SE Concavity", 
                     "SE Concave Points", "SE Symmetry", "SE Fractal Dimension", 
                     "Worst Radius", "Worst Texture", "Worst Perimeter", "Worst Area", "Worst Smoothness", "Worst Compactness", 
                     "Worst Concavity", "Worst Concave Points", "Worst Symmetry", "Worst Fractal Dimension")

# Exibir as primeiras linhas e a estrutura do dataset
head(dados)
str(dados)

# Remover a coluna 'ID' (não necessária para análise)
dados <- dados[-1]
str(dados)

# Verificar se há valores ausentes
any(is.na(dados))

# Transformando a variável 'Diagnosis' em um fator com rótulos 'Benigno' e 'Maligno'
dados$Diagnosis <- factor(dados$Diagnosis, levels = c("B", "M"), labels = c("Benigno", "Maligno"))
str(dados)

# Verificando a proporção das classes
round(prop.table(table(dados$Diagnosis)) * 100, digits = 1)

# Normalização das variáveis numéricas
summary(dados[c("Mean Radius", "Mean Area", "Mean Smoothness")])

# Função para normalizar os dados
normalizar <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Testando a função de normalização
normalizar(c(1, 2, 3, 4, 5, 6))
normalizar(c(10, 20, 30, 40, 50, 60))

# Aplicando a normalização no dataframe (somente nas variáveis numéricas)
dados_normalizados <- as.data.frame(lapply(dados[2:31], normalizar))

# Verificando se os dados foram normalizados corretamente
summary(dados[c("Mean Radius", "Mean Area", "Mean Smoothness")])
summary(dados_normalizados[c("Mean.Smoothness", "Mean.Area", "Mean.Radius")])

# Dividindo os dados em treino e teste
dados_treino <- dados_normalizados[1:469, ]
dados_teste <- dados_normalizados[470:569, ]

# Criando os labels para o treino e teste
dados_treino_labels <- dados[1:469, 1]
dados_teste_labels <- dados[470:569, 1]

# Treinando o modelo KNN com k = 21
install.packages("class") # Instala o pacote se necessário
library(class)
modelo <- knn(train = dados_treino, test = dados_teste, cl = dados_treino_labels, k = 21)

# Exibindo a proporção das classes no conjunto de dados de treino e teste
round(prop.table(table(dados$Diagnosis)) * 100, digits = 1)
round(prop.table(table(dados_teste_labels)) * 100, digits = 1)

# Verificando a classe do modelo
class(modelo)

# Analisando o desempenho do modelo com uma matriz de confusão
install.packages("gmodels") # Instala o pacote se necessário
library(gmodels)

# Matriz de confusão
#Teste commit
CrossTable(x = dados_teste_labels, y = modelo, prop.chisq = FALSE)
round(prop.table(table(dados$Diagnosis)) * 100, digits = 1)
