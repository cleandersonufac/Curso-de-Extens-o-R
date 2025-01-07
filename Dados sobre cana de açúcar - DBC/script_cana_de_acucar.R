# Dados sobre Cana-de-açúcar
# A diagnose foliar é uma ferramenta essencial para a avaliação nutricional das plantas, sendo amplamente utilizada para monitorar os teores de nutrientes em cultivos agrícolas como a cana-de-açúcar. Neste estudo, foi realizado um experimento para avaliar o impacto de diferentes métodos de preparo de amostras foliares no teor de cobre (ppm) presente nas folhas da cultura de cana-de-açúcar. O objetivo principal foi identificar o tratamento mais eficiente na obtenção de amostras representativas, garantindo precisão nos resultados analíticos.
# 
# Para tal, foram considerados cinco tratamentos distintos no preparo das folhas de cana-de-açúcar:
#   
#   - **T1**: Folhas sem sofrerem qualquer tipo de limpeza;
# - **T2**: Folhas submetidas à passagem de escova acoplada a aspirador de pé;
# - **T3**: Folhas lavadas em água corrente e enxaguadas em água destilada e desmineralizada;
# - **T4**: Folhas lavadas em solução diluída de detergente (ODD a 0,1%), seguidas por enxágue com água destilada, imersão em HCl 0,1 N e lavagem final com água desmineralizada;
# - **T5**: Folhas lavadas em solução diluída de detergente (ODD a 0,1%), enxaguadas em água destilada até completa remoção do detergente e lavadas com água desmineralizada.
# 
# Os diferentes métodos de limpeza e preparo foram aplicados para avaliar como o processo influencia a remoção de impurezas e resíduos que poderiam interferir na análise química. A variável analisada foi o teor de cobre (ppm), um micronutriente essencial para o desenvolvimento das plantas, mas que pode apresentar contaminações externas devido ao manejo ou ao ambiente.

# Criando o data.frame com os dados fornecidos
dados_cn <- data.frame(
  Tratamento = rep(c("T1", "T2", "T3", "T4", "T5"), each = 8),  # Tratamentos
  Bloco = rep(1:8, times = 5),  # 8 blocos
  Teores = c(11.5, 12.7, 12.6, 12.2, 10.4, 12.0, 12.2,  8.5,
             7.7,  9.0,  9.1,  8.6,  8.8,  8.6,  8.4,  8.4,
             9.8,  8.0,  7.4,  9.5,  8.3,  8.9, 10.5, 10.4,
             10.7, 10.8, 10.2,  9.6,  9.8, 10.1, 11.1, 11.5,
             12.0, 10.9, 10.3,  9.8,  9.4,  9.5,  9.7, 10.1)
)

# Visualizar os dados
print(dados_cn)

# Carregar pacotes necessários
library(knitr)        # Para criar tabelas formatadas
library(kableExtra)   # Para melhorar a apresentação das tabelas
library(AgroR)        # Para construir o croqui (se necessário)
library(ExpDes.pt)    # Para a análise de variâncias
library(ggplot2)      # Para os gráficos

## Croqui experimental 
set.seed(1234)
# Construção do croqui do experimento usando o pacote AgroR
# O croqui mostra a disposição dos tratamentos nos blocos
Trat <- paste("Trat", 1:5)
sketch(Trat, r = 8, design = "dbc")  # 5 blocos

## Estatísticas Descritiva


summary(dados_cn)
aggregate(Teores ~ Tratamento, data = dados_cn, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(Teores ~ Bloco, data = dados_cn, FUN = function(x) c(mean = mean(x), sd = sd(x)))

## Análises gráfica

### Perfil

# Todos os perfis juntos

# Criar o gráfico para os dados de gordura absorvida
ggplot(dados_cn, aes(x = Bloco, y = Teores, group = Tratamento)) +
  geom_line(aes(linetype = Tratamento), 
            size = 0.5) +  # Linhas conectando os pontos por tipo de gordura
  geom_point(aes(color = Tratamento), 
             size = 1.5) +    # Pontos para cada valor de gordura absorvida
  scale_x_continuous(breaks = 1:8, 
                     labels = paste("B", 1:8)) +  # Eixo X representando os Blocos
  stat_summary(fun = mean, geom = "line", lwd = 1.0, 
               aes(group = Tratamento,
                   color = Tratamento)) +  # Média por tipo
  xlab("Blocos") +
  ylab("Teores") +
  theme_bw() +  # Tema com fundo branco
  theme(axis.text.x = element_text(size = 13)) +  # Tamanho do texto no eixo X
  theme(axis.text.y = element_text(size = 13)) 

# perfis separados

# Criar o gráfico para os dados de cana-de-açúcar
ggplot(dados_cn, aes(x = Bloco, y = Teores, group = Tratamento)) +
  geom_line(aes(linetype = Tratamento), 
            size = 0.5) +  # Linhas conectando os pontos por tipo de gordura
  geom_point(aes(color = Tratamento), 
             size = 1.5) +    # Pontos para cada valor de teor de cobre
  scale_x_continuous(breaks = 1:8, 
                     labels = paste("B", 1:8)) +  # Eixo X representando os lotes
  stat_summary(fun = mean, geom = "line", lwd = 1.0, 
               aes(group = Tratamento,
                   color = Tratamento)) +  # Média por tipo
  xlab("Blocos") +
  ylab("Teores") +
  theme_bw() +  # Tema com fundo branco
  theme(axis.text.x = element_text(size = 8)) +  # Tamanho do texto no eixo X
  theme(axis.text.y = element_text(size = 10)) +  # Tamanho do texto no eixo Y
  facet_wrap(~Tratamento)  # Dividir o gráfico por tratamento


## Box-plots

# Gráfico de boxplot para visualizar a distribuição dos teores de cobre por tratamento
ggplot(dados_cn, aes(x = Tratamento, y = Teores, 
                     fill = Tratamento)) +
  geom_boxplot() +
  labs(title = "Distribuição de Teores de Cobre por Tratamento",
       x = "Tratamentos",
       y = "Teores de Cobre (ppm)") +
  theme_minimal()


## Anova

# Realizar a ANOVA com o pacote ExpDes.pt
# Inclui o efeito dos blocos no modelo
dbc(
  trat = dados_cn$Tratamento, 
  bloco = dados_cn$Bloco, 
  resp = dados_cn$Teores, 
  quali = TRUE,  # Tratamento é qualitativo
  mcomp = "tukey",  # Teste de comparação múltipla: Tukey
  sigT = 0.05,      # Nível de significância para Tukey
  sigF = 0.05       # Nível de significância para ANOVA
)


## Verificação dos pressupostos

# Verificar os pressupostos do modelo ANOVA
modelo <- aov(Teores ~ as.factor(Tratamento) + as.factor(Bloco), 
              data = dados_cn)  # Ajustar modelo ANOVA

# Plotar gráficos de resíduos
par(mfrow = c(1, 2))  # Configurar para dois gráficos lado a lado
plot(modelo, which = 1)  # Resíduos vs Ajustados
plot(modelo, which = 2)  # QQ plot dos resíduos
par(mfrow = c(1, 1)) 

## Utilizando o teste Scott-Knot

# Teste post-hoc: Scott-Knott (usando ExpDes.pt)
dbc(
  trat = dados_cn$Tratamento, 
  bloco = dados_cn$Bloco, 
  resp = dados_cn$Teores, 
  quali = TRUE, 
  mcomp = "sk",  # Teste Scott-Knott
  sigT = 0.05, 
  sigF = 0.05
)
