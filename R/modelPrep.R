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


# seed
set.seed(28)

# Divisão treino-test
index <- createDataPartition(data_agrupado$condition, p = 0.80, list = FALSE)
treino_limpo <- data_agrupado[index, ]
teste_limpo  <- data_agrupado[-index, ]