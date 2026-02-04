# Pegando dados
data2= read.csv(here::here("data", "heart_cleveland_upload.csv"))


# Transformando variáveis categóricas em fatores
cols_para_fator <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
data2[cols_para_fator] <- lapply(data2[cols_para_fator], as.factor)

data2$condition <- factor(data2$condition,
                         levels = c(0, 1),
                         labels = c("Saudavel", "Doente"))

data_limpo <- data2 %>%
  mutate(
    cp = case_when(
      cp %in% c(0, 1, 2) ~ "_com dor",
      cp == 3            ~ "_sem dor"
    ),

    slope = case_when(
      slope == 0      ~ "_asc",
      slope %in% c(1, 2) ~ "_not_asc"
    ),

    ca = case_when(
      ca == 0         ~ "_zero",
      ca %in% c(1, 2, 3) ~ "_not_zero"
    ),

    thal = case_when(
      thal == 0      ~ "_normal",
      thal %in% c(1, 2) ~ "_not_normal"
    ),
    restecg = case_when(
      restecg == 0      ~ "_normal",
      restecg %in% c(1, 2) ~ "_not_normal"
    )
  ) %>%
  # Garante que tudo volte a ser fator para o modelo
  mutate(across(c(cp, slope, ca, thal, restecg), as.factor))

# seed
set.seed(28)

# Divisão treino-test
index <- createDataPartition(data_limpo$condition, p = 0.80, list = FALSE)
treino_limpo <- data_limpo[index, ]
teste_limpo  <- data_limpo[-index, ]





