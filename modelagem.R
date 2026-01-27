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
ic_perfil <- confint(modelo_sem, level = 0.95)
print(ic_perfil)
## Vemos que algumas variáveis possuem ICs com grande amplitude;
## Essas variáveis são justamente as variáveis indicadoras com pouca
## representatividade no dataset;








# Análise de resíduos do modelo reduzido
residuos_deviance <- residuals(modelo_reduzido_, type = "deviance")
residuos_pearson <- residuals(modelo_reduzido_, type = "pearson")


# Criar o QQ-Plot
qqnorm(residuos_deviance, main = "QQ-Plot dos Resíduos Deviance (Modelo Reduzido)")
qqline(residuos_deviance, col = "red", lwd = 2)


plot(modelo_reduzido_)
plot_deviance_residuals(modelo_reduzido_, "Modelo 6 Variáveis")


## nesses gráficos, é possível ver que há alguns pontos discrepantes em que o modelo
## tem dificuldade de classificar
nomes_outliers <- names(sort(abs(residuos_pearson), decreasing = TRUE)[1:5])

treino_sem_outliers <- treino[!(rownames(treino) %in% nomes_outliers), ]
modelo_sem <- glm(condition ~ sex + cp + exang + oldpeak + slope + ca + thal,
                  data = treino_sem_outliers, family = "binomial")

coef_comp = cbind(Com_Outliers = coef(modelo_reduzido_),
                  Sem_Outliers = coef(modelo_sem))
print(coef_comp)







resultados <- resamples(list(modelo_14_variaveis = modelo1, modelo_6_variaveis = modelo2))
metricas_folds <- resultados$values


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


# Aplicando para o Modelo 1 e Modelo 2
metricas_m1 <- calcular_metricas_custom(modelo1$pred, melhor_threshold1) %>%
  mutate(Modelo = "Modelo_14_var")

metricas_m2 <- calcular_metricas_custom(modelo2$pred, melhor_threshold2) %>%
  mutate(Modelo = "Modelo_6_var")

# Unindo os resultados
metricas_ajustadas <- bind_rows(metricas_m1, metricas_m2)



tabela_comparativa_final <- gerar_tabela_estatistica(metricas_ajustadas)

View(tabela_comparativa_final)
plot_comparativo_metricas(tabela_comparativa_final)

## É possível observar que o desempenho dos dois modelos é similar, mas o modelo com 6 variáveis é
## mais estável (intervalos de confiança de menor amplitude)
## Pelo princípio da parcimônia, optamos pelo modelo reduzido














summary(modelo_sem)
# AIC: 122.43



# Criando árove usando as variáveis do modelo reduzido
modelo_arvore <- rpart(condition ~ sex + cp + oldpeak + slope + ca + thal, 
                       data = treino_sem_outliers, 
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

View(exp(cbind(OR = coef(modelo_reduzido_), confint(modelo_reduzido_))))
# O intervalo de confinança de algumas variáveis indicadoras está muito grande
# Principalmente as variáveis indicadoras com pouca representatividade no dataset;







