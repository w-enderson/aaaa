# Pegando dados
data2= read.csv('./heart_cleveland_upload.csv')

head(data2)

# Transformando variáveis categóricas em fatores
cols_para_fator <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
data2[cols_para_fator] <- lapply(data2[cols_para_fator], as.factor)

data2$condition <- factor(data2$condition,
                         levels = c(0, 1),
                         labels = c("Saudavel", "Doente"))

data_limpo <- data2 %>%
  mutate(
    # cp: 0, 1 e 2 viram "Baixo_Risco", 3 vira "Alto_Risco"
    cp = case_when(
      cp %in% c(0, 1, 2) ~ "Baixo_Risco",
      cp == 3            ~ "Alto_Risco"
    ),
    # slope: 0 vira "Subida", 1 e 2 viram "Descida" (ou vice-versa conforme sua análise)
    slope = case_when(
      slope == 0      ~ "Nivel_0",
      slope %in% c(1, 2) ~ "Nivel_1_2"
    ),
    # ca: 0 vira "Zero", 1, 2 e 3 viram "Um_ou_Mais"
    ca = case_when(
      ca == 0         ~ "Zero",
      ca %in% c(1, 2, 3) ~ "Um_ou_Mais"
    ),
    # thal: 0 vira "Normal", 1 e 2 viram "Defeito"
    thal = case_when(
      thal == 0      ~ "Normal",
      thal %in% c(1, 2) ~ "Defeito"
    ),
    # restecg: 0 vira "Normal", 1 e 2 viram "Anormal"
    restecg = case_when(
      restecg == 0      ~ "Normal",
      restecg %in% c(1, 2) ~ "Anormal"
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


modelo_limpo <- glm(condition ~ sex+ cp +
                   oldpeak + slope + ca + thal, 
                 data = treino_limpo, 
                 family = "binomial")
Anova(modelo_limpo, type = "III", test = "LR")

summary(modelo_limpo)
plot(modelo_limpo)

# Análise de resíduos do modelo reduzido
residuos_pearson <- residuals(modelo_limpo, type = "pearson")

# Criar o QQ-Plot
qqnorm(residuos_deviance, main = "QQ-Plot dos Resíduos Deviance (Modelo Reduzido)")
qqline(residuos_deviance, col = "red", lwd = 2)
## A deviance é mais próximo de uma normal, do que no modelo nos dados originais


# IC 95% dos parâmetros do modelo com outliers
ic_parametros <- cbind(
  Estimativa = coef(modelo_limpo),
  confint(modelo_limpo)
)
print(round(ic_parametros, 4))

## Esse modelo possui parâmetros mais estáveis e menores que o modelo anterior;
## Mesmo que o modelo anterior tenha AIC menor, ele está atribuindo muito valor a
## Determinadas variáveis


# Criando árove usando as variáveis do modelo reduzido
modelo_arvore <- rpart(condition ~ sex+ cp +
                         oldpeak + slope + ca + thal, 
                       data = treino_limpo, 
                       method = "class") 

# Árvore criada
rpart.plot(modelo_arvore, 
           type = 4, 
           extra = 104, 
           under = TRUE, 
           cex = 0.8, 
           box.palette = "RdBu", 
           main = "Árvore de Decisão: Identificando Interações")



# Avaliação no conjunto de teste - Regressão Logística
prob_teste <- predict(modelo_limpo, newdata = teste_limpo, type = "response")
threshold_final <- 0.5 
pred_classe_teste <- factor(ifelse(prob_teste >= threshold_final, "Doente", "Saudavel"), 
                            levels = c("Saudavel", "Doente"))

# Matriz de Confusão
conf_matrix <- confusionMatrix(pred_classe_teste, teste_limpo$condition, positive = "Doente")
print(conf_matrix)



## Avaliação na árvore de decisão
pred_arvore_classe <- predict(modelo_arvore, newdata = teste_limpo, type = "class")
prob_arvore <- predict(modelo_arvore, newdata = teste_limpo, type = "prob")[, "Doente"]

# Matriz de Confusão
conf_matrix_arvore <- confusionMatrix(pred_arvore_classe, teste_limpo$condition, positive = "Doente")
print(conf_matrix_arvore)


