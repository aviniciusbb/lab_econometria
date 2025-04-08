# Aula Lab - Introducao a Econometria
# Regressao linear e teste de hipotese
# Data: 19/09/2024


# Carregar pacotes
library(tidyverse)
library(broom)


# Carregar dados
# Inserir caminho correto do arquivo no seu computador
censo_pop <- read_csv("~/Documents/lab_econometria/salarios.csv")

# Analisar dados
glimpse(censo_pop)



# Salario por nivel de educacao
censo_pop |> 
  ggplot() +
  geom_point(
    aes(x = educ, y = salario)
  ) +
  theme_minimal()



# Regressao linear -----

# Salario e anos de escolaridade

reg_linear <- lm(salario ~ educ, data = censo_pop) 
summary(reg_linear)

# Alternativamente
reg_linear_tidy <- lm(salario ~ educ, data = censo_pop) |> tidy()
reg_linear_tidy

# Adicionando a reta de regressao
censo_pop |> 
  ggplot() +
  geom_point(
    aes(x = educ, y = salario)
  ) +
  geom_smooth(
    aes(x = educ, y = salario), color = "red", 
    lwd = 1.5, method = lm, se = FALSE) +
  theme_minimal()


# Intervalo de confianca -----

alpha <- 0.05 # nivel de confianca
gl <- nrow(censo_pop) - 2 # graus de liberdade (n-2)
beta_1 <- reg_linear_tidy$estimate[2] # coeficiente beta_1
se_beta_1 <- reg_linear_tidy$std.error[2] # erro padrao de beta_1
tc <- qt(1 - alpha/2, gl) # valor t critico


# Limite inferior
lim_inf <- beta_1 - tc*se_beta_1

# Limite superior
lim_sup <- beta_1 + tc*se_beta_1

# Intervalo de confianca
ic <- c(lim_inf, lim_sup)
ic

# Alternativamente
confint(reg_linear)


# Distribuicao dos coeficientes

coef_sample <- vector(mode = "list")
set.seed(12345)

for (i in 1:10000){
  
  sample <- censo_pop |> 
    sample_n(200) 
  
  reg_sample <- lm(salario ~ educ, data = sample) |> tidy()
  
  
  coef_sample[[i]] <- reg_sample$estimate[2]
  
  
}

# Distribuicao dos coeficientes
unlist(coef_sample) |> 
  as_tibble() |> 
  rename(coeficiente = value) |> 
  ggplot() +
  geom_density(aes(x = coeficiente)) +
  theme_minimal()



# Distribuicao da estatistica t

ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dt, args = list(df = 28)) +
  stat_function(fun = dt, args = list(df = 28),
                xlim = c(2.05,4),
                geom = "area", fill = "red") +
  stat_function(fun = dt, args = list(df = 28),
                xlim = c(-2.05,-4),
                geom = "area", fill = "red") +
  geom_vline(xintercept = reg_sample$statistic[2], col = "blue") +
  theme_minimal()



# Reportanto resultados em tabelas
stargazer::stargazer(reg_linear, type = "text")


