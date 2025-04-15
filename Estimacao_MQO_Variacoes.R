# Aula - Introducao a Econometria
# Analise de regressao
# Data: 26/04/2024


# Carregar pacotes
install.packages("tidyverse")
library(tidyverse)

# Modelo com variavies dummy -----

# Carregar dados
dados_salarios <- read_csv("dados/salarios.csv")

# Analisar dados
glimpse(dados_salarios)


# Estatisticas descritivas -----

# Salario medio por genero
dados_salarios |> 
  group_by(mulher) |> 
  summarise(
    salario_medio = mean(salario)
  )

# Salario medio por raca
dados_salarios |> 
  group_by(negro) |> 
  summarise(
    salario_medio = mean(salario)
  )


# Visualizacao -----

# Salario medio por genero
dados_salarios |> 
  mutate(mulher = as.factor(mulher)) |> 
  group_by(mulher) |> 
  summarise(
    salario_medio = mean(salario)
  ) |> 
  ggplot() +
  geom_col(
    aes(x =  mulher, y = salario_medio, fill = mulher)
  ) +
  scale_fill_manual(values = c("#00A5E0", "#EF9CDA")) +
  xlab("Gênero") + ylab("Salário médio") +
  ggtitle("Salário médio por gênero") +
  theme_minimal()



# Salario medio por raca
dados_salarios |> 
  mutate(negro = as.factor(negro)) |> 
  group_by(negro) |> 
  summarise(
    salario_medio = mean(salario)
  ) |> 
  ggplot() +
  geom_col(
    aes(x = negro, y = salario_medio, fill = negro)
  ) +
  scale_fill_manual(values = c("#EBEFBF", "#92828D")) +
  xlab("Raça") + ylab("Salário médio") +
  ggtitle("Salário médio por raca") +
  theme_minimal()





# Regressao - dummy de genero
reg_genero <- lm(salario ~ mulher, data = dados_salarios)
summary(reg_genero)


# Regressao - dummy de raca/cor
reg_raca <- lm(salario ~ negro, data = dados_salarios)
summary(reg_raca)


# Adicionando dummies de intercepto e interacao  

# Analise por genero

reg_genero_educ <- lm(salario ~ educ, data = dados_salarios)
summary(reg_genero_educ)

reg_genero_educ_comp <- lm(salario ~ educ + mulher + educ:mulher, data = dados_salarios)
summary(reg_genero_educ_comp)


# Analise por raca e educacao

reg_raca_educ_comp <- lm(salario ~ educ + negro + educ:negro, data = dados_salarios)
summary(reg_raca_educ_comp)



# Modelos nao-lineares -----

# Ler dados (inserir caminho correto)
censo_br <- read_csv("dados/censo_br.csv")

# Listar variaveis
glimpse(censo_br)


# Filtrar para o ano de 2010
censo_br_2010 <- censo_br |> 
  filter(ano == 2010) |> 
  mutate(idhm = idhm * 100)


# Parte 1 - Relacao entre renda e IDHM

censo_br_2010 |>
  ggplot() +
  geom_point(aes(x = rdpc, y = idhm,
                 color = regiao)) +
  ggtitle("RDPC vs. IDHM")


# Adicionar linha de regressao

censo_br_2010 |>
  ggplot() +
  geom_point(aes(x = rdpc, y = idhm), 
             color = "darkblue") +
  geom_smooth(aes(x = rdpc, y = idhm),
              color = "orange", se = FALSE, method = "lm") +
  ggtitle("RDPC vs. IDHM")


# Regressao
modelo_1 <- lm(idhm ~ rdpc, data = censo_br_2010)
summary(modelo_1)


# Utilizando o modelo log-log

censo_br_2010 |>
  ggplot() +
  geom_point(aes(x = log(rdpc), y = log(idhm)),
             color = "darkblue") +
  ggtitle("RDPC vs. IDHM")


# Adicionar linha de regressao

censo_br_2010 |>
  ggplot() +
  geom_point(aes(x = log(rdpc), y = log(idhm))
             , color = "darkblue") +
  geom_smooth(aes(x = log(rdpc), y = log(idhm)),
              color = "orange", se = FALSE, method = "lm") +
  ggtitle("RDPC vs. IDHM")


# Regressao
modelo_2 <- lm(log(idhm) ~ log(rdpc), data = censo_br_2010)
summary(modelo_2)


# Analise por regiao -----

censo_br_2010 |>
  ggplot() +
  geom_point(aes(x = log(rdpc), y = log(idhm),
                 color = regiao)) +
  ggtitle("RDPC vs. IDHM") +
  facet_wrap(~regiao)

# Regiao nordeste
modelo_ne <- lm(log(idhm) ~ log(rdpc), 
                data = filter(censo_br_2010, regiao == "Nordeste"))
summary(modelo_ne)

# Regiao sul
modelo_s <- lm(log(idhm) ~ log(rdpc), 
               data = filter(censo_br_2010, regiao == "Sul"))
summary(modelo_s)

# Regiao sudeste
modelo_se <- lm(log(idhm) ~ log(rdpc), 
                data = filter(censo_br_2010, regiao == "Sudeste"))
summary(modelo_se)


# Regiao norte
modelo_n <- lm(log(idhm) ~ log(rdpc), 
                data = filter(censo_br_2010, regiao == "Norte"))
summary(modelo_n)








