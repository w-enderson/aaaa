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
index <- createDataPartition(data_limpo$condition, p = 0.80, list = FALSE)
treino_limpo <- data_limpo[index, ]
teste_limpo  <- data_limpo[-index, ]