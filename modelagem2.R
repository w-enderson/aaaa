# Pegando dados
data2= read.csv('./heart_cleveland_upload.csv')

head(data2)

# Transformando variáveis categóricas em fatores
cols_para_fator <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
data2[cols_para_fator] <- lapply(data2[cols_para_fator], as.factor)

data2$condition <- factor(data2$condition,
                         levels = c(0, 1),
                         labels = c("Saudavel", "Doente"))


                         library(dplyr)

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
treino <- data_limpo[index, ]
teste  <- data_limpo[-index, ]


modelo1_2 <- glm(formula1, data = treino, family = "binomial")
Anova(modelo1_2, type = "III", test = "LR")
modelo_reduzido_2 <- glm(condition ~   cp + oldpeak + slope+
                          ca + thal,
                    data = treino, 
                    family = "binomial")
anova(modelo1_2, modelo_reduzido_2, test = "Chisq")



# Análise de resíduos do modelo reduzido
residuos_deviance <- residuals(modelo_reduzido_2, type = "deviance")

# Criar o QQ-Plot
qqnorm(residuos_deviance, main = "QQ-Plot dos Resíduos Deviance (Modelo Reduzido)")
qqline(residuos_deviance, col = "red", lwd = 2)
plot(modelo_reduzido_2)


summary(modelo_reduzido_2)
View(exp(cbind(OR = coef(modelo_reduzido_2), confint(modelo_reduzido_2))))





# Criando árove usando as variáveis do modelo reduzido
modelo_arvore <- rpart(condition ~ cp + oldpeak + slope+
                          ca + thal, 
                       data = treino, 
                       method = "class") 

# Árvore criada
rpart.plot(modelo_arvore, 
           type = 4, 
           extra = 104, 
           under = TRUE, 
           cex = 0.8, 
           box.palette = "RdBu", 
           main = "Árvore de Decisão: Identificando Interações")




# --- 1. Modelo Logístico Reduzido ---
# Obtendo probabilidades e convertendo para classes (threshold padrão 0.5)
prob_glm <- predict(modelo_reduzido_2, newdata = teste, type = "response")
pred_glm <- factor(ifelse(prob_glm > 0.5, "Doente", "Saudavel"), levels = c("Saudavel", "Doente"))

# Matriz de Confusão GLM
mc_glm <- confusionMatrix(pred_glm, teste$condition, positive = "Doente")

# --- 2. Modelo de Árvore de Decisão ---
# Predição direta da classe
pred_arvore <- predict(modelo_arvore, newdata = teste, type = "class")

# Matriz de Confusão Árvore
mc_arvore <- confusionMatrix(pred_arvore, teste$condition, positive = "Doente")





df_comparativo <- data.frame(
  Metrica = c("Acurácia", "Sensibilidade", "Especificidade", "Kappa"),
  GLM_Reduzido = c(mc_glm$overall["Accuracy"], mc_glm$byClass["Sensitivity"], 
                    mc_glm$byClass["Specificity"], mc_glm$overall["Kappa"]),
  Arvore_Decisao = c(mc_arvore$overall["Accuracy"], mc_arvore$byClass["Sensitivity"], 
                      mc_arvore$byClass["Specificity"], mc_arvore$overall["Kappa"])
)

print(df_comparativo)