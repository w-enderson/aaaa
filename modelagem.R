

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

## Todas as variáveis são importantes e trazem novas informações que ajudam na classificação

set.seed(28)
modelo2 <- train( condition ~  sex + cp +
                        oldpeak + slope + ca + thal,
                  data = treino,
                  method = "glm",
                  family = "binomial",
                  trControl = ctrl)

resultados <- resamples(list(modelo_14_variaveis = modelo1, modelo_6_variaveis = modelo2))
metricas_folds <- resultados$values

summary(resultados)
dotplot(resultados)

## É possível observar que o modelo com 6 variáveis possui um poder de discriminação
## maior, em relação ao modelo com 14 variáveis (AUC, prAUC)
## O modelo de 6 variáveis também é mais estável



lista_de_modelos <- list(modelo_14_var = modelo1, modelo_6_var = modelo2)

# Chame a nova função
analise_otimizada <- analisar_modelos_otimizados(lista_de_modelos)

# Veja os resultados agora com os thresholds aplicados!
View(analise_otimizada$tabela)
print(analise_otimizada$grafico)


## O modelo reduzido é mais preciso (intervalo de confiança com menor amplitude), em relação ao modelo 
## completo; Pelo princípio da parcimônia, optamos pelo uso do modelo reduzido




# Análise de resíduos do modelo reduzido
residuos_deviance <- residuals(modelo_reduzido_, type = "deviance")
residuos_pearson <- residuals(modelo_reduzido_, type = "pearson")

# Criar o QQ-Plot
qqnorm(residuos_deviance, main = "QQ-Plot dos Resíduos Deviance (Modelo Reduzido)")
qqline(residuos_deviance, col = "red", lwd = 2)


plot(residuos_deviance)
plot(residuos_pearson)


plot(modelo_reduzido_)

## É possível observar que há alguns candidatos a outliers (observações com maiores deviances)

## Para verificar o impacto dessas observações, treinaremos um modelo sem essas observações para
## verficar se eles impactam significativamente os coeficientes da regrressão



# Identificar quais índices têm deviance absoluta maior que 3
outliers_index <- which(abs(residuos_deviance) > 2.5)

# Ver quantos são e quais são
print(outliers_index)

# Se quiser ver esses dados na tabela original
casos_estranhos <- treino[outliers_index, ]
View(casos_estranhos)





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
