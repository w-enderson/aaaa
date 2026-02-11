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








calc_boot_metrics <- function(data, indices, modelo, tipo_modelo) {
  # Reamostragem
  d <- data[indices, ]
  
  # Predições
  if (tipo_modelo == "glm") {
    probs <- predict(modelo, d, type = "response")
    # Ajuste para garantir que os níveis do fator estejam corretos
    preds <- factor(ifelse(probs > 0.5, levels(d$condition)[2], levels(d$condition)[1]), 
                    levels = levels(d$condition))
  } else {
    probs <- predict(modelo, d, type = "prob")[, 2]
    preds <- predict(modelo, d, type = "class")
  }
  
  # Cálculos usando nomes completos dos pacotes para evitar conflitos
  cm <- caret::confusionMatrix(preds, d$condition)
  
  # Especificamos pROC::roc e pROC::auc para não confundir com o pacote precrec
  roc_obj <- pROC::roc(d$condition, probs, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  # PR-AUC
  true_numeric <- ifelse(d$condition == levels(d$condition)[2], 1, 0)
  pr_auc_val <- MLmetrics::PRAUC(y_pred = probs, y_true = true_numeric)
  
  return(c(
    Acc = cm$overall['Accuracy'],
    Prec = cm$byClass['Precision'],
    Rec = cm$byClass['Recall'],
    AUC = auc_val,
    PR_AUC = pr_auc_val
  ))
}

# --- 3. FUNÇÃO PARA ORGANIZAR RESULTADOS DO BOOSTRPA ---

summarize_boot <- function(boot_obj, nome_modelo) {
  metrics_names <- c("Acurácia", "Precisão", "Recall", "AUC", "PR-AUC")
  res <- data.frame(
    Modelo = nome_modelo,
    Metrica = metrics_names,
    Media = apply(boot_obj$t, 2, mean),
    IC_Lower = apply(boot_obj$t, 2, function(x) quantile(x, 0.025)),
    IC_Upper = apply(boot_obj$t, 2, function(x) quantile(x, 0.975))
  )
  return(res)
}



plot_logit_linearity <- function(modelo, variavel_continua, data, n_labels = 10) {
  df_plot <- data
  df_plot$prob_predita <- predict(modelo, newdata = df_plot, type = "response")
  
  # Logit (Log-Odds)
  df_plot$logito <- log(df_plot$prob_predita / (1 - df_plot$prob_predita))
  
  # 1. Calcular resíduos lineares para identificar os pontos mais distantes da tendência
  # Ajustamos uma reta temporária para medir a distância (resíduo)
  temp_lm <- lm(as.formula(paste("logito ~", variavel_continua)), data = df_plot)
  df_plot$residuos_abs <- abs(residuals(temp_lm))
  
  # 2. Marcar os top 'n' pontos mais distantes
  df_plot <- df_plot %>%
    mutate(label_ponto = ifelse(rank(-residuos_abs) <= n_labels, 
                                as.character(row.names(df_plot)), ""))

  # 3. Gerar o gráfico
  ggplot(df_plot, aes_string(x = variavel_continua, y = "logito")) +
    geom_point() +
    geom_smooth(method = "lm", color = "darkblue", linetype = "dashed") + # Reta de Mínimos Quadrados
    geom_smooth(method = "loess", color = "firebrick", fill = "gray80") + # Tendência Local
    geom_text_repel(aes(label = label_ponto), size = 3, fontface = "bold") +
    labs(
      title = paste("Linearidade e Outliers no Logito:", variavel_continua),
      subtitle = paste("Marcando os", n_labels, "pontos com maior resíduo"),
      x = variavel_continua,
      y = "Log-Odds (Logito)",
    ) +
    theme_minimal()
}

# plot_logit_linearity(modelo_agrupado, "oldpeak", treino_agrupado)