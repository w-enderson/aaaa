gerar_tabela_or <- function(modelo) {
  
  # odds ratios 
  ors <- exp(stats::coef(modelo))
  
  # p-values
  p_values <- summary(modelo)$coefficients[,4]

  # intervalos de confiança (nível de 5%)
  intervalos <- suppressMessages(exp(stats::confint(modelo)))
  

  tabela <- data.frame(
    "OR" = ors,
    "IC_2.5" = intervalos[,1],
    "IC_97.5" = intervalos[,2],
    "amplitude" = intervalos[,2] - intervalos[,1],
    "p_valor" = p_values
  ) 

  tabela <- round(tabela, 4)
  
  return(tabela)
}


# Função para calcular a métrica em cada sorteio (boot)
calc_all_metrics <- function(data, indices) {
  amostra <- data[indices, ]
  
  # Predições
  preds_prob <- predict(modelo_agrupado, newdata = amostra, type = "response")
  preds_class <- factor(ifelse(preds_prob > 0.5, "Doente", "Saudavel"), levels = c("Saudavel", "Doente"))
  real <- factor(amostra$condition, levels = c("Saudavel", "Doente"))
  
  # Matriz de Confusão
  cm <- confusionMatrix(preds_class, real, positive = "1")
  
  # Extraindo o que você pediu:
  acuracia <- cm$overall["Accuracy"]
  kappa    <- cm$overall["Kappa"]
  sens     <- cm$byClass["Sensitivity"]
  espec    <- cm$byClass["Specificity"]
  f1       <- cm$byClass["F1"]
  
  return(c(acuracia, kappa, sens, espec, f1))
}


validar_modelo_bootstrap <- function(modelo, dados_teste, nome_coluna_alvo, R = 1000) {
  
  # 1. Função interna para o cálculo das métricas
  calc_metrics <- function(data, indices) {
    amostra <- data[indices, ]
    
    # Predições de probabilidade e classe
    preds_prob <- predict(modelo, newdata = amostra, type = "response")
    # Garante que os níveis sejam 0 e 1 para a Matrix de Confusão
    preds_class <- factor(ifelse(preds_prob > 0.5, "Doente", "Saudavel"), levels = c("Saudavel", "Doente"))
    real <- factor(amostra[[nome_coluna_alvo]], levels = c("Saudavel", "Doente"))
    
    # Cálculo da Confusion Matrix
    cm <- confusionMatrix(preds_class, real, positive = "Doente")
    
    # Vetor de retorno: Acurácia, Kappa, Sensibilidade, Especificidade, F1
    return(c(
      Acuracia = cm$overall["Accuracy"],
      Kappa    = cm$overall["Kappa"],
      Sens     = cm$byClass["Sensitivity"],
      Espec    = cm$byClass["Specificity"],
      F1       = cm$byClass["F1"]
    ))
  }
  
  # 2. Executa o Bootstrap
  set.seed(123) # Para reprodutibilidade
  boot_res <- boot(data = dados_teste, statistic = calc_metrics, R = R)
  
  # 3. Organiza a Tabela Final
  nomes_metrics <- c("Acurácia", "Kappa", "Sensibilidade", "Especificidade", "F1-Score")
  tabela <- data.frame(
    Metrica = nomes_metrics,
    Media = numeric(5),
    DP = numeric(5),
    IC_Inf = numeric(5),
    IC_Sup = numeric(5)
  )
  
  for (i in 1:5) {
    tabela$Media[i] <- mean(boot_res$t[, i])
    tabela$DP[i]    <- sd(boot_res$t[, i])
    
    # Tenta calcular o IC (pode falhar se a variância for zero em algum fold)
    try({
      ci <- boot.ci(boot_res, type = "perc", index = i)
      tabela$IC_Inf[i] <- ci$percent[4]
      tabela$IC_Sup[i] <- ci$percent[5]
    }, silent = TRUE)
  }
  
  # Arredondamento para 4 casas
  tabela[,-1] <- round(tabela[,-1], 4)
  
  return(tabela)
}
