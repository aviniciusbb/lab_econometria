# Introducao a Econometria
# Aula: Regressao Linear Multipla
# Antonio Vinicius

# Carregar pacotes
library(tidyverse)

# Ler dados

aval_professor <- readr::read_csv(
  "dados/aval_professor.csv"
)

glimpse(aval_professor)

# Distribuicao de notas
aval_professor |> 
  ggplot() +
  geom_histogram(
    aes(x = score), binwidth = .215,
    color = "black", fill = "gray"
  ) +
  theme_minimal()


# Relacao com outras variaveis

#  Relacao entre avaliacao e genero
aval_professor |> 
  ggplot() +
  geom_boxplot(
    aes(x = gender, y = score, fill = gender)
  ) +
  theme_minimal()

#  Relacao entre avaliacao e etnia
aval_professor |> 
  ggplot() +
  geom_boxplot(
    aes(x = ethnicity, y = score, fill = ethnicity)
  ) +
  theme_minimal()

#  Relacao entre beleza e genero
aval_professor |> 
  ggplot() +
  geom_boxplot(
    aes(x = gender, y = bty_avg, fill = gender)
  ) +
  theme_minimal()


# Regressao linear simples -----

# Grafico de dispersao
aval_professor |> 
  ggplot() +
  geom_point(
    aes(x = bty_avg, y = score),
    color = "purple"
  ) +
  theme_minimal()

# Modelo
mod_bty <- lm(score ~ bty_avg, data = aval_professor)
summary(mod_bty)

# Analisando os residuos
data_residuos <- data.frame(
  residuos = mod_bty$residuals,
  bty_avg = aval_professor$bty_avg
)

data_residuos |> 
  ggplot() +
  geom_point(
    aes(x = bty_avg, y = residuos)
  ) +
  geom_hline(yintercept = 0, lty = 2, color = "red")


# Regressao linear multiplo -----

aval_professor <- aval_professor |> 
  dplyr::mutate(
    d_gender = if_else(gender == "male", 1, 0)
  )


# Adicionando o genero do professor
mod_bty_genero <- lm(score ~ bty_avg + d_gender, 
                     data = aval_professor)
summary(mod_bty_genero)

aval_professor |> 
  ggplot() +
  geom_point(
    aes(bty_avg, y = score, color = gender)
  ) +
  scale_colour_manual(values = c("#E29578", "#006D77")
  ) +
  theme_minimal() +
  # Reta de regressao para 'female'
  geom_abline(
    intercept = mod_bty_genero$coefficients[1], 
    slope = mod_bty_genero$coefficients[2],
    color = "#E29578", lwd = 1
  ) +
  # Reta de regressao para 'male'
  geom_abline(
    intercept = mod_bty_genero$coefficients[1] + mod_bty_genero$coefficients[3], 
    slope = mod_bty_genero$coefficients[2],
    color = "#006D77", lwd = 1
  )


# Adicionando o rank do professor

# Tenured: professor titular
# Tenure track: em busca da titularidade
# Teaching: professor assistente

mod_bty_rank <- lm(score ~ bty_avg + rank, 
                   data = aval_professor)
summary(mod_bty_rank)


aval_professor <- aval_professor |> 
  dplyr::mutate(
    d_teaching = if_else(rank == "teaching", 1, 0),
    d_tenure_track = if_else(rank == "tenure track", 1, 0),
    d_tenured = if_else(rank == "tenured", 1, 0)
  )

mod_bty_rank <- lm(score ~ bty_avg + d_teaching + d_tenured, 
                   data = aval_professor)
summary(mod_bty_rank)


aval_professor |> 
  ggplot() +
  geom_point(
    aes(bty_avg, y = score, color = rank)
  ) +
  scale_colour_manual(values = c("#FFBC42", "#8F2D56", "#73D2DE")
  ) +
  theme_minimal() +
  # Reta de regressao para 'teaching'
  geom_abline(
    intercept = mod_bty_rank$coefficients[1], 
    slope = mod_bty_rank$coefficients[2],
    color = "#FFBC42", lwd = 1
  ) +
  # Reta de regressao para 'tenure track'
  geom_abline(
    intercept = mod_bty_rank$rank[1] + mod_bty_rank$coefficients[3], 
    slope = mod_bty_rank$coefficients[2],
    color = "#8F2D56", lwd = 1
  ) +
  # Reta de regressao para 'tenure track'
geom_abline(
  intercept = mod_bty_rank$rank[1] + mod_bty_rank$coefficients[4], 
  slope = mod_bty_rank$coefficients[2],
  color = "#73D2DE", lwd = 1
)


# Modelo completo -----

mod_full <- lm(score ~ rank + ethnicity + gender + language +
  age + cls_perc_eval + cls_students + cls_level + bty_avg, 
data = aval_professor)
summary(mod_full)



# Prevendo gastos medicos ----

seguro_saude <- readr::read_csv(
  "dados/insurance.csv"
)

glimpse(seguro_saude)


# Histograma de gastos
seguro_saude |> 
  ggplot() +
  geom_histogram(
    aes(x = expenses),
    color = "black", fill = "gray"
  ) +
  geom_vline(
    xintercept = mean(seguro_saude$expenses),
    color = "red", lwd = 2, lty = 2
  ) +
  geom_vline(
    xintercept = median(seguro_saude$expenses),
    color = "blue", lwd = 2, lty = 2
  ) +
  theme_minimal()

# Matriz de correlacao
cor(seguro_saude[c("age", "bmi", "children", "expenses")])


# Relacao entre variaveis

# Idade e gastos com saude
seguro_saude |> 
  ggplot() +
  geom_point(
    aes(x = age, y = expenses)
  ) +
  theme_minimal()

# IMC e gastos com saude
seguro_saude |> 
  ggplot() +
  geom_point(
    aes(x = bmi, y = expenses)
  ) +
  theme_minimal()


# Modelo

mod_gastos <- lm(expenses ~ age + children + bmi + 
  sex + smoker + region, data = seguro_saude)
summary(mod_gastos)


mod_gastos <- lm(log(expenses) ~ age + children + log(bmi) + 
  sex + smoker + region,  data = seguro_saude)
summary(mod_gastos)

