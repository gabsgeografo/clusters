# UEPG - Programa de Pos-Graduacao em Agronomia
# Metodos em Estatista Multivariada

################################################################################
####                     Análise de Agrupamento (Clusters) - Pacote 'Metan'
################################################################################


library(metan)

#' # Carregar dados
dados <- read.csv("Euroemp.csv", header = T, row.names = 1);  # A coluna '1' sera lida como rotulo
head(dados)
df <- scale(dados[,-1]) # Padronização dos dados (media = 0, variancia = 1)

d1 <- clustering(dados[,-1],
                 scale = TRUE,
                 clustmethod = "mcquitty",
                 nclust = 4)
plot(d1, horiz = TRUE, ylab = "Distância euclidiana")


# Coeficiente de Correlacao Cofenetica
d1$cophenetic
