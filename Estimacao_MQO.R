# Introducao a Econometria
# Aula: Regressao Linear Simples - Estimacao por MQO
# Antonio Vinicius


# Carregar pacotes -----
library(tidyverse)


# -------------------------------------- #
# Exemplo 1: Renda e gastos das familias #
# -------------------------------------- #

# Abrir dados 
dados_gastos <- read_csv("dados/dados_gastos.csv")

# Analisando os dados 
glimpse(dados_gastos)


# Ajustar dados
dados_gastos <- dados_gastos |> 
  mutate(income = income * 100)


# Visualizacao - grafico de dispersao
dados_gastos |> 
  ggplot() +
  geom_point(
    aes(x = income, y = food_exp)
    )


# Covariancia e correlacao entre as variaveis
cov(dados_gastos$food_exp, dados_gastos$income)
cor(dados_gastos$food_exp, dados_gastos$income)


# Correlacao - alternativo
cov(dados_gastos$food_exp, dados_gastos$income) / 
  (sd(dados_gastos$food_exp) * sd(dados_gastos$income))


# Obter estimadores 'manualmente'
beta_1 <- cov(dados_gastos$food_exp, dados_gastos$income)/var(dados_gastos$income)
beta_0 <- mean(dados_gastos$food_exp) - beta_1 * mean(dados_gastos$income)


# Estimacao do modelo de MQO
modelo <- lm(food_exp ~ income, data = dados_gastos) 
summary(modelo)
modelo$residuals

# Adicionar reta de regressao 
dados_gastos |> 
  ggplot() +
  geom_point(
    aes(x = income, y = food_exp)
    ) +
  # geom_abline(
  #   intercept = beta_0,
  #   slope = beta_1,
  #   lwd = 2,
  #   color = "green"
  # )
# Alternativamente
  geom_smooth(
   aes(x = income, y = food_exp),
   color = "blue",
   method = lm, se = FALSE
  )


# Analise dos residuos 

residuo_df <- data.frame(
  income = dados_gastos$income,
  residuo = modelo$residuals
)

View(residuo_df)

# Plot de residuos
residuo_df |> 
  ggplot() +
  geom_point(
    aes(x = income, y = residuo)
    ) +
  geom_hline(
    yintercept = 0, color = "red", lwd = 1
    )


# Distribuicao dos residuos 

#* Histograma
residuo_df |> 
  ggplot() +
  geom_histogram(
    aes(x = residuo), color = "white"
  ) +
  xlim(-400, 400)

#* Densidade kernel
residuo_df |> 
  ggplot() +
  geom_density(
    aes(x = residuo), fill = "orange"
  ) +
  xlim(-400, 400)

# Estatisticas descritrivas dos desvios
sum(residuo_df$residuo)
mean(residuo_df$residuo)
sd(residuo_df$residuo)


# Coeficiente de determinacao R2 -----

# R2 = 1 - (SQR/SQT)
R2 <- 1 - sum(modelo$residuals^2) / 
  sum((dados_gastos$food_exp - mean(dados_gastos$food_exp))^2)

# R2 = quadrado do coeficiente correlacao
R2_alt <- cor(dados_gastos$food_exp, dados_gastos$income)^2



