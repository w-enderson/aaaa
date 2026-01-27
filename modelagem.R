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
summary(modelo1_) ## AIC : 183.26

# Estatísticas do cross validation
print(modelo1) ## Acc : .8565


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


# IC baseado em Perfil de Verossimilhança 
ic_perfil <- confint(modelo_reduzido_, level = 0.95)
print(ic_perfil)
## Vemos que algumas variáveis possuem ICs com grande amplitude;
## Essas variáveis são justamente as variáveis indicadoras com pouca
## representatividade no dataset;







## nesses gráficos, é possível ver que há alguns pontos discrepantes em que o modelo
## tem dificuldade de classificar
nomes_outliers <- names(sort(abs(residuos_pearson), decreasing = TRUE)[1:5])

treino_sem_outliers <- treino[!(rownames(treino) %in% nomes_outliers), ]
modelo_sem <- glm(condition ~ sex + cp + exang + oldpeak + slope + ca + thal,
                  data = treino_sem_outliers, family = "binomial")

coef_comp = cbind(Com_Outliers = coef(modelo_reduzido_),
                  Sem_Outliers = coef(modelo_sem))
print(coef_comp)



summary(modelo_sem)
# AIC: 122.43











