
analisar_modelos_otimizados <- function(lista_modelos) {
  
  resultados_finais <- list()
  
  for(nome_modelo in names(lista_modelos)) {
    mod <- lista_modelos[[nome_modelo]]
    preds <- mod$pred
    
    # 1. Calcular a curva ROC global para achar o melhor threshold
    roc_obj <- roc(response = preds$obs, 
                   predictor = preds$Doente, 
                   levels = c("Saudavel", "Doente"),
                   quiet = TRUE)
    
    best_t <- coords(roc_obj, "best", ret = "threshold")[[1]]
    
    # 2. Recalcular métricas por Resample (Fold) usando o melhor threshold
    metricas_por_fold <- preds %>%
      group_by(Resample) %>%
      summarise(
        ROC       = as.numeric(auc(roc(obs, Doente, quiet = TRUE))),
        # Aplicando o threshold otimizado aqui:
        Pred_Class = list(factor(ifelse(Doente >= best_t, "Doente", "Saudavel"), 
                                 levels = c("Saudavel", "Doente"))),
        Obs        = list(obs)
      ) %>%
      mutate(
        # Extraindo as métricas binárias após o novo corte
        Accuracy  = map2_dbl(Pred_Class, Obs, ~postResample(.x, .y)[1]),
        Kappa     = map2_dbl(Pred_Class, Obs, ~postResample(.x, .y)[2]),
        F1        = map2_dbl(Pred_Class, Obs, ~ {
          cm <- confusionMatrix(.x, .y, positive = "Doente")
          cm$byClass["F1"]
        }),
        Recall    = map2_dbl(Pred_Class, Obs, ~ {
          cm <- confusionMatrix(.x, .y, positive = "Doente")
          cm$byClass["Recall"]
        }),
        Precision = map2_dbl(Pred_Class, Obs, ~ {
          cm <- confusionMatrix(.x, .y, positive = "Doente")
          cm$byClass["Precision"]
        })
      ) %>%
      select(-Pred_Class, -Obs) %>%
      pivot_longer(cols = -Resample, names_to = "Metrica", values_to = "Valor") %>%
      mutate(Modelo = nome_modelo, Threshold = best_t)
    
    resultados_finais[[nome_modelo]] <- metricas_por_fold
  }
  
  # Consolidação da Tabela Comparativa
  tabela_comparativa <- bind_rows(resultados_finais) %>%
    group_by(Modelo, Metrica, Threshold) %>%
    summarise(
      Min      = min(Valor, na.rm = TRUE),
      Q025     = quantile(Valor, probs = 0.025, na.rm = TRUE),
      Media    = mean(Valor, na.rm = TRUE),
      DP       = sd(Valor, na.rm = TRUE),
      Q975     = quantile(Valor, probs = 0.975, na.rm = TRUE),
      Max      = max(Valor, na.rm = TRUE),
      .groups  = 'drop'
    ) %>%
    mutate(across(where(is.numeric), ~ round(., 4)))
  
  # Geração do Gráfico
  grafico <- ggplot(tabela_comparativa, aes(x = Media, y = Modelo, color = Modelo)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = Q025, xmax = Q975), height = 0.2) + 
    facet_wrap(~Metrica, scales = "free_x") +
    theme_minimal() +
    labs(title = "Comparação de Modelos com Threshold Otimizado (IC 95%)",
         subtitle = "Métricas recalculadas individualmente com o melhor ponto de corte de cada modelo",
         x = "Valor Médio (Fold CV)", y = NULL)
  
  return(list(tabela = tabela_comparativa, grafico = grafico))
}
