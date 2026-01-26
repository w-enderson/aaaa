# seed
set.seed(28)

# Divisão treino-test
index <- createDataPartition(data$condition, p = 0.80, list = FALSE)
treino <- data[index, ]
teste  <- data[-index, ]

# cross validation
ctrl <- trainControl( method = "cv", 
                      number = 10, 
                      classProbs = TRUE, 
                      savePredictions = "final",
                      summaryFunction = multiClassSummary,)


## Modelo 1 - com todas as variáveis
formula1 <- condition ~ age + sex + cp + thalach + exang +
                         oldpeak + slope + ca + thal +
                         trestbps + chol + fbs + restecg
modelo1 <- train(formula1, 
                  data = treino, 
                  method = "glm", 
                  family = "binomial", 
                  trControl = ctrl,
                  )
modelo1_ <- glm(formula1, data = treino, family = "binomial")

# Ajuste do modelo
summary(modelo1_)

# Estatísticas do cross validation
print(modelo1)


# Teste de Razão de Verossimilhança
Anova(modelo1_, type = "III", test = "LR")

## As variáveis : restecg, fbs, exang, age, thalac, trestbps e chol
## possuem os maiores p-values do teste, indicando que elas não contribuem
## significativamente para a classificação.


# Avaliando se a remoção dessas 7 variáveis impacta significativamente
# o modelo

# Modelo reduzido (sem as 7 )
modelo_reduzido_ <- glm(condition ~  sex + cp +
                        oldpeak + slope + ca + thal,
                    data = treino, family = "binomial")

# O teste de grupo
anova(modelo1_, modelo_reduzido_, test = "Chisq")

## O P-valor do teste foi 0.7863, indicando que não há diferença
## significativa entre o modelo com todas variáveis e o reduzido.


# Teste de Razão de Verossimilhança para o modelo reduzido
Anova(modelo_reduzido_, type = "III", test = "LR")

## Todas as variáveis parecem ser significativas

set.seed(28)
modelo2 <- train( condition ~  sex + cp +
                        oldpeak + slope + ca + thal,
                  data = treino,
                  method = "glm",
                  family = "binomial",
                  trControl = ctrl)

resultados <- resamples(list(modelo_14_variaveis = modelo1, modelo_6_variaveis = modelo2))
summary(resultados)
dotplot(resultados)

## É possível observar que o modelo com 6 variáveis possui um poder de discriminação
## maior, em relação ao modelo com 14 variáveis (AUC, prAUC)


# Definindo melhor threshold para ambos os modelos

preds_cv1 <- modelo1$pred
roc_cv1 <- roc(response = preds_cv1$obs, 
              predictor = preds_cv1$Doente,
              levels = c("Saudavel", "Doente"))
melhor_threshold1 <- coords(roc_cv, "best", ret = "threshold")[[1]]
cat("O threshold ótimo definido no CV é:", melhor_threshold1, "\n")


preds_cv2 <- modelo2$pred
roc_cv2 <- roc(response = preds_cv2$obs, 
              predictor = preds_cv2$Doente,
              levels = c("Saudavel", "Doente"))
melhor_threshold2 <- coords(roc_cv2, "best", ret = "threshold")[[1]]
cat("O threshold ótimo definido no CV é:", melhor_threshold2, "\n")


