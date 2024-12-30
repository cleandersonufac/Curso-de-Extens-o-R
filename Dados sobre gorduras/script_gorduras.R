## O problema das rosquinhas

# Durante o cozimento, as rosquinhas absorvem gordura em diferentes quantidades. Este estudo busca determinar se a **quantidade de gordura absorvida** pelas rosquinhas depende do tipo de gordura utilizada no cozimento. Foram avaliados:
#   
# - **4 tipos de gordura** (\( TG_1, TG_2, TG_3, TG_4 \)).
# - **6 lotes** de rosquinhas por tipo de gordura.
# - Cada lote é composto por 24 rosquinhas.
# 
# Os dados são expressos em gramas de gordura absorvida por lote, subtraindo-se 100 g para simplificação. A variável **tipo de gordura** é o único fator de classificação neste experimento.

## Dados de Gordura Absorvida
  
# Carregar pacotes necessários
library(knitr)  # Para criar tabelas formatadas
library(kableExtra)  # Para melhorar a apresentação das tabelas
library(AgroR) # Para construir o Croqui
library(ExpDes.pt) # Para a análise de variâncias
library(ggplot2) # Para os gráficos 
library(lattice) # Para os gráficos

# Dados de gordura absorvida
dados <- data.frame(
  Tipo_de_gordura = c("TG1", "TG2", "TG3", "TG4"),
  Lote_1 = c(64, 78, 75, 55),
  Lote_2 = c(72, 91, 93, 66),
  Lote_3 = c(68, 97, 78, 49),
  Lote_4 = c(77, 82, 71, 64),
  Lote_5 = c(56, 85, 63, 70),
  Lote_6 = c(95, 77, 76, 68)
)

# Adicionar totais e médias
dados$Total <- rowSums(dados[2:7])  # Soma por linha (Total de cada tipo de gordura)
dados$Media <- round(rowMeans(dados[2:7]), 2)  # Média de cada tipo de gordura

# Total e média geral
total_geral <- sum(dados$Total)  # Soma total
media_geral <- round(mean(dados$Media), 2)  # Média geral

# Adicionar linha de total geral e média geral
dados_geral <- rbind(
  dados,
  c("Geral", "-", "-", "-", "-", "-", "-", total_geral, media_geral)
)

# Exibir a tabela formatada
kable(dados_geral, align = "c", col.names = c("Tipo de gordura", "Lote 1", "Lote 2", "Lote 3",
                                              "Lote 4", "Lote 5", "Lote 6", "Total", "Média")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))


## Leitura dos dados no R

# Criando o data.frame com os dados fornecidos
dados_rosquinhas <- data.frame(
  Tipo_de_gordura = rep(c("TG1", "TG2", "TG3", "TG4"), each = 6),  # Tipos de gordura
  Lote = rep(1:6, times = 4),  # Lotes de 1 a 6 para cada tipo
  Gordura_absorvida = c(
    64, 72, 68, 77, 56, 95,  # Gordura absorvida por TG1
    78, 91, 97, 82, 85, 77,  # Gordura absorvida por TG2
    75, 93, 78, 71, 63, 76,  # Gordura absorvida por TG3
    55, 66, 49, 64, 70, 68   # Gordura absorvida por TG4
  )
)

# Visualizar os dados
print(dados_rosquinhas)
kableExtra::kable(dados_rosquinhas)

## Estatística Descritiva

# As médias por tratamento são... 

# Estatísticas descritivas: Resumo geral e por tratamento
summary(dados_rosquinhas)
aggregate(Gordura_absorvida ~ Tipo_de_gordura, data = dados_rosquinhas, FUN = function(x) c(mean = mean(x), sd = sd(x)))

## Análise gráfica 

# Visualização do experimento

# Criar o gráfico para os dados de gordura absorvida
ggplot(dados_rosquinhas, aes(x = Lote, y = Gordura_absorvida, group = Tipo_de_gordura)) +
  geom_line(aes(linetype = Tipo_de_gordura), size = 0.5) +  # Linhas conectando os pontos por tipo de gordura
  geom_point(aes(color = Tipo_de_gordura), size = 1.5) +    # Pontos para cada valor de gordura absorvida
  scale_x_continuous(breaks = 1:6, labels = paste("Lote", 1:6)) +  # Eixo X representando os lotes
  stat_summary(fun = mean, geom = "line", lwd = 1.0, aes(group = Tipo_de_gordura, color = Tipo_de_gordura)) +  # Média por tipo
  xlab("Lote") +
  ylab("Gordura Absorvida (g)") +
  theme_bw() +  # Tema com fundo branco
  theme(axis.text.x = element_text(size = 13)) +  # Tamanho do texto no eixo X
  theme(axis.text.y = element_text(size = 13)) +  # Tamanho do texto no eixo Y
  facet_wrap(~Tipo_de_gordura)  # Dividir o gráfico por tipo de gordura

# Gráfico de boxplot para visualizar a distribuição da gordura absorvida por tratamento
ggplot(dados_rosquinhas, aes(x = Tipo_de_gordura, y = Gordura_absorvida, 
                             fill = Tipo_de_gordura)) +
  geom_boxplot() +
  labs(title = "Distribuição de Gordura Absorvida por Tratamento",
       x = "Tipo de Gordura",
       y = "Gordura Absorvida (g)") +
  theme_minimal()

## Anova

# Realizar a ANOVA com o pacote ExpDes.pt
# Verifica se há diferença significativa entre os tratamentos
dic(
  trat = dados_rosquinhas$Tipo_de_gordura, 
  resp = dados_rosquinhas$Gordura_absorvida, 
  quali = TRUE,  # Tratamento é qualitativo
  mcomp = "tukey",  # Teste de comparação múltipla: Tukey
  sigT = 0.05,      # Nível de significância para Tukey
  sigF = 0.05       # Nível de significância para ANOVA
)

## Verificação dos pressupostos

# Verificar os pressupostos do modelo ANOVA
modelo <- aov(Gordura_absorvida ~ Tipo_de_gordura, data = dados_rosquinhas)  # Ajustar modelo ANOVA

# Teste de normalidade dos resíduos (Shapiro-Wilk)
shapiro.test(residuals(modelo))  # p > 0.05 indica normalidade

# Teste de homogeneidade de variâncias (Bartlett)
bartlett.test(Gordura_absorvida ~ Tipo_de_gordura, 
              data = dados_rosquinhas)  # p > 0.05 indica homogeneidade

# Plotar gráficos de resíduos
par(mfrow = c(1, 2))  # Configurar para dois gráficos lado a lado
plot(modelo, which = 1)  # Resíduos vs Ajustados
plot(modelo, which = 2)  # QQ plot dos resíduos
par(mfrow = c(1,1))

## Utilizando o teste Scott-Knot

# Teste post-hoc: Scott-Knott (usando ExpDes.pt)
dic(
  trat = dados_rosquinhas$Tipo_de_gordura, 
  resp = dados_rosquinhas$Gordura_absorvida, 
  quali = TRUE, 
  mcomp = "sk",  # Teste Scott-Knott
  sigT = 0.05, 
  sigF = 0.05
)

