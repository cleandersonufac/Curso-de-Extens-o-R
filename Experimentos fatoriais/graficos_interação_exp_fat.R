# Gráficos para experimentos fatoriais

## Exemplo 1

# Sejam os dados de um experimento, em blocos casualizados, no esquema fatorial \(3 \times 3\), em que foram estudados os efeitos de **3 Peneiras comerciais**, associadas a **3 Densidades de plantio**, na produtividade do amendoim (*Arachis hypogaea L.*) variedade Tatu V53.
# 
# As **Peneiras comerciais (P)** e as **Densidades de plantio (D)** estudadas foram:
#   
#   - \(P_1\): peneira 18 (crivos circulares com diâmetro de 18/64 polegadas);
# - \(P_2\): peneira 20 (crivos circulares com diâmetro de 20/64 polegadas);
# - \(P_3\): peneira 22 (crivos circulares com diâmetro de 22/64 polegadas);
# 
# - \(D_1\): 10 plantas por metro linear;
# - \(D_2\): 15 plantas por metro linear;
# - \(D_3\): 20 plantas por metro linear.

# Caso 1: Experimento sem interação
# Conjunto de Dados
cat("### Caso 1: Experimento sem interação\n")
dados_amendoim <- data.frame(
  Tratamentos = c("P1D1", "P1D2", "P1D3", "P2D1", "P2D2", "P2D3", "P3D1", "P3D2", "P3D3"),
  Peneira = rep(c("P1", "P2", "P3"), each = 3),
  Densidade = rep(c("D1", "D2", "D3"), times = 3),
  Blocos = paste("Bloco", rep(rep(1:3,3),3)),
  Producao = c(11.82, 12.34, 13.41, 6.97, 8.96, 8.48, 7.53, 6.71, 6.82,
               12.03, 14.08, 12.98, 10.26, 9.2, 9.66, 7.67, 7.87, 8.44,
               12.55, 12.13, 13.35, 9.02, 9.84, 8.50, 7.81, 9.49, 9.37)
)

print(dados_amendoim)

## Croqui experimental

library(AgroR)
sketch(trat=c("P1","P2","P3"),#paste("D",rep(1:3,3)),
       trat1=c("D1","D2","D3"),#paste("P",rep(1:3,3)),
       design = "fat2dbc",
       r=3)


## Construção do gráfico de interação


# Calculando a produção média por tratamento
dados_media <- aggregate(Producao ~ Peneira + Densidade, data = dados_amendoim, FUN = mean)

# Visualizando os dados com médias
print(dados_media)

# Gráfico de Interação com Produção Média
library(ggplot2)
ggplot(dados_media, aes(x = Densidade, y = Producao, color = Peneira, group = Peneira)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Gráfico de Interação: Peneiras x Densidades",
    x = "Densidade de Plantio",
    y = "Produção Média (g/planta)"
  ) +
  theme_minimal()

As linhas paralelas indicam ausência de interação significativa entre os fatores.


## Exemplo 2

# Considerando os dados de um experimento inteiramente casualizado, com **4 repetições** no esquema fatorial, para testar os efeitos de **3 Recipientes** (\(R_1\), \(R_2\) e \(R_3\)) e **2 Espécies de eucaliptos** (\(E_1\) e \(E_2\)), na produção de mudas. 
# 
# Quanto ao desenvolvimento das mudas, os recipientes e as espécies testados foram:
#   
#   - \(R_1\): saco plástico pequeno;
# - \(R_2\): saco plástico grande;
# - \(R_3\): laminado;
# - \(E_1\): *Eucalyptus citriodora*;
# - \(E_2\): *Eucalyptus grandis*.

# Caso 2: Experimento com interação
# Conjunto de Dados
# Criando os dados do experimento
dados_eucalipto <- data.frame(
  Tratamentos = c("R1E1", "R1E2", "R2E1", "R2E2", "R3E1", "R3E2"),
  Recepiente = rep(c("R1", "R2", "R3"), each = 2),
  Especie = rep(c("E1", "E2"), times = 3),
  Altura = c(26.2, 24.8, 25.7, 19.6, 22.8, 19.8,
             26.0, 24.6, 26.3, 21.1, 19.4, 21.4,
             25.0, 26.7, 25.1, 19.0, 18.8, 22.8,
             25.4, 25.2, 26.4, 18.6, 19.2, 21.3)
)

# Visualizando os dados
print(dados_eucalipto)

## Croqui experimental

sketch(trat=c("E1","E2"),
       trat1=c("R1","R2","R3"),
       design = "fat2dic",
       r=4)


## Gráfico para interação


# Calculando a altura média por combinação de Recipiente e Espécie
library(dplyr)

dados_media <- dados_eucalipto %>%
  group_by(Recepiente, Especie) %>%
  summarise(Media_Altura = mean(Altura, na.rm = TRUE))

# Visualizando os dados com médias
print(dados_media)

# Gráfico de Interação com Alturas Médias
library(ggplot2)
ggplot(dados_media, aes(x = Especie, y = Media_Altura, 
                        color = Recepiente, 
                        group = Recepiente)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Gráfico de Interação: Recipientes x Espécies",
    x = "Espécie",
    y = "Altura Média (cm)"
  ) +
  theme_minimal()


# As linhas não paralelas (e, neste caso, cruzadas) indicam a presença de interação entre os fatores.

