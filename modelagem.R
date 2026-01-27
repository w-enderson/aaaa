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

## As variáveis : age, thalach, exang, trestbps, chol, fbs, restecg
## possuem os maiores p-values do teste, indicando que elas não contribuem
## significativamente para a classificação.


# Para verificar isso, usaremos o teste de razão de verossimilhança 
# Entre o modelo completo e o reduzido


# Modelo reduzido ( com 6 variáveis )
modelo_reduzido_ <- glm(condition ~  sex + cp +
                        oldpeak + slope + ca + thal,
                    data = treino, 
                    family = "binomial")

# O teste de grupo
anova(modelo1_, modelo_reduzido_, test = "Chisq")

## O P-valor do teste foi 0.7863, indicando que não há diferença
## significativa entre o modelo com todas variáveis e o reduzido.


# Teste de Razão de Verossimilhança para o modelo reduzido
Anova(modelo_reduzido_, type = "III", test = "LR")

## Todas as variáveis são importantes e trazem informações importantes

set.seed(28)
modelo2 <- train( condition ~  sex + cp +
                        oldpeak + slope + ca + thal,
                  data = treino,
                  method = "glm",
                  family = "binomial",
                  trControl = ctrl)

resultados <- resamples(list(modelo_14_variaveis = modelo1, modelo_6_variaveis = modelo2))
metricas_folds <- resultados$values

#summary(resultados)
#dotplot(resultados)

## É possível observar que o modelo com 6 variáveis possui um poder de discriminação
## maior, em relação ao modelo com 14 variáveis (AUC, prAUC)


# Definindo melhor threshold para ambos os modelos

preds_cv1 <- modelo1$pred
roc_cv1 <- roc(response = preds_cv1$obs, 
              predictor = preds_cv1$Doente,
              levels = c("Saudavel", "Doente"))
melhor_threshold1 <- coords(roc_cv1, "best", ret = "threshold")[[1]]
# 0.4594418 

preds_cv2 <- modelo2$pred
roc_cv2 <- roc(response = preds_cv2$obs, 
              predictor = preds_cv2$Doente,
              levels = c("Saudavel", "Doente"))
melhor_threshold2 <- coords(roc_cv2, "best", ret = "threshold")[[1]]
# 0.3934961 


tabela_comparativa <- metricas_folds %>%
    # Transformar de formato largo para longo
    pivot_longer(cols = -Resample, 
                 names_to = "Modelo_Metrica", 
                 values_to = "Valor") %>%
    
    # Separar o nome do modelo da métrica
    separate(Modelo_Metrica, into = c("Modelo", "Metrica"), sep = "~") %>%
    
    # Filtrar apenas as métricas de interesse
    filter(Metrica %in% c("Accuracy", "F1", "Recall", "Precision", "ROC", "prAUC")) %>%
    
    # Agrupar para calcular as estatísticas
    group_by(Modelo, Metrica) %>%
    summarise(
        Min      = min(Valor, na.rm = TRUE),
        Q025     = quantile(Valor, probs = 0.025, na.rm = TRUE),
        Media    = mean(Valor, na.rm = TRUE),
        Mediana  = median(Valor, na.rm = TRUE),
        DP       = sd(Valor, na.rm = TRUE),
        Q975     = quantile(Valor, probs = 0.975, na.rm = TRUE),
        Max      = max(Valor, na.rm = TRUE),
        .groups = 'drop'
    ) %>%
    
    # Formatar para exibição
    mutate(across(where(is.numeric), ~ round(., 4)))

View(as.data.frame(tabela_comparativa))

## O modelo reduzido é mais estável (desvio padrão baixo), em relação ao modelo 
## completo; Pelo princípio da parcimônia, optamos pelo modelo reduzido


# Análise de resíduos do modelo reduzido
residuos_deviance <- residuals(modelo_reduzido_, type = "deviance")

# Criar o QQ-Plot
qqnorm(residuos_deviance, main = "QQ-Plot dos Resíduos Deviance (Modelo Reduzido)")
qqline(residuos_deviance, col = "red", lwd = 2)



plot(modelo_reduzido_)


# Criando árove usando as variáveis do modelo reduzido
modelo_arvore <- rpart(condition ~ sex + cp + oldpeak + slope + ca + thal, 
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



summary(modelo_reduzido_)

# Extrai Odds Ratio e Intervalo de Confiança de 95%
View(exp(cbind(OR = coef(modelo_reduzido_), confint(modelo_reduzido_))))

table(treino$ca, treino$condition)


cat("--- PERFORMANCE GLM REDUZIDO ---\n")
print(confusionMatrix(pred_glm, teste$condition, positive = "Doente")$overall["Accuracy"])

cat("\n--- PERFORMANCE GLM INTERAÇÃO ---\n")
print(confusionMatrix(pred_int, teste$condition, positive = "Doente")$overall["Accuracy"])

cat("\n--- PERFORMANCE ÁRVORE ---\n")
print(confusionMatrix(pred_arvore, teste$condition, positive = "Doente")$overall["Accuracy"])


importancia <- varImp(modelo2, scale = FALSE)
plot(importancia, main = "Importância das Variáveis - Modelo Reduzido")