# limpando o ambiente
rm(list = ls())

# Carregando os pacotes
library(ExpDes)

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

dados_amendoim <- data.frame(
  Tratamentos = c("P1D1", "P1D2", "P1D3", "P2D1", "P2D2", "P2D3", "P3D1", "P3D2", "P3D3"),
  Peneira = as.factor(rep(c("P1", "P2", "P3"), each = 3)),
  Densidade = as.factor(rep(c("D1", "D2", "D3"), times = )),
  Blocos = as.factor(rep(c("Bloco1", "Bloco2", "Bloco3"), each = 9)),
  Producao = c(11.82, 12.34, 13.41, 6.97, 8.96, 8.48, 7.53, 6.71, 6.82,
               12.03, 14.08, 12.98, 10.26, 9.2, 9.66, 7.67, 7.87, 8.44,
               12.55, 12.13, 13.35, 9.02, 9.84, 8.50, 7.81, 9.49, 9.37)
)

print(dados_amendoim)

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

# As linhas paralelas indicam ausência de interação significativa entre os fatores.

# Croqui Experimental
library(AgroR)
sketch(trat=c("P1","P2","P3"),#paste("D",rep(1:3,3)),
       trat1=c("D1","D2","D3"),#paste("P",rep(1:3,3)),
       design = "fat2dbc",
       r=3)

# Análise de variância utilizando o pacote ExpDes.pt

attach(dados_amendoim)

fat2.dbc(
  Peneira,
  Densidade,
  Blocos,
  Producao,
  quali = c(TRUE, TRUE),
  mcomp = "tukey",
  fac.names = c("Peneira", "Densidade"),
  sigT = 0.05,
  sigF = 0.05
)

detach(dados_amendoim)

# Análise de Variância utilizando o pacote AgroR

library(AgroR)
with(dados_amendoim,
     FAT2DBC(f1 = Densidade, 
             f2 = Peneira, 
             block = Blocos, 
             Producao, 
             ylab="Produção", 
             xlab = "Trat",
             names.fat = c("Densidade", "Peneira"),
             legend = "Densidade",
             fill = "trat"))




