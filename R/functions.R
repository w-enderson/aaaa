

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