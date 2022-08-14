
# Carregando Pacotes
library(genalg)
library(ggplot2)

# Definindo o caminho
setwd('C:/Users/joagu/OneDrive - SENAC - SP/CoisasdoJoao/Curso/Data Science/Meus_Projetos/Pesquisa_Operacional/Meu_Exemplo')

df <- read.csv('dados.csv')
df$X = NULL

# Verificando Dataset
View(df)
str(df)

# Preço limite
orcamento <- 190
alcool_max <- 28

# Criando função
# Função de Avaliação
evalFunc <- function(x) {
  qualidade <- x %*% df$qualidade_prevista # Soma a qualidade
  preco <- x %*% df$preco # Soma o preco
  alcool <- x %*% df$alcohol # Soma o alcool
  
  if (preco > orcamento & alcool > alcool_max)
    return(0) else return(-qualidade)
}

# Aplicando o algoritimo
iter = 100
GAmodel <- rbga.bin(size = 6, popSize = 200, iters = iter, mutationChance = 0.01, elitism = T, evalFunc = evalFunc)
cat(summary(GAmodel, echo=TRUE))

# Aplicando a solução encontrada
solution = c(1, 0, 0, 1, 1, 0)
df[solution == 1, ]


# Verificando a qualidade maxima atingida
cat(paste(solution %*% df$qualidade_prevista, "/", sum(df$qualidade_prevista)))

# Verificando o preço pelo orçamento
cat(paste(solution %*% df$preco, "/", orcamento))

# Verificando o alcool
cat(paste(solution %*% df$alcohol, "/", alcool_max))

# Plot
plot(GAmodel)
