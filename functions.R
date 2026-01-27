calcular_metricas_custom <- function(preds, threshold) {
  preds %>%
    group_by(Resample) %>%
    summarise(
      # Transforma probabilidade em classe baseada no melhor threshold
      pred_class = factor(ifelse(Doente >= threshold, "Doente", "Saudavel"), 
                          levels = c("Saudavel", "Doente")),
      obs = factor(obs, levels = c("Saudavel", "Doente")),
      
      # Cálculo das métricas
      Accuracy  = postResample(pred_class, obs)[1],
      Recall    = sensitivity(pred_class, obs, positive = "Doente"),
      Precision = posPredValue(pred_class, obs, positive = "Doente"),
      F1        = (2 * Precision * Recall) / (Precision + Recall),
      .groups = "drop"
    ) %>%
    # Remove duplicatas se houver e mantém apenas as colunas de métricas
    distinct()
}





plot_comparativo_metricas <- function(dados, titulo = "Comparação de Métricas com IC 95%") {
  
  # Garantir que as métricas apareçam em uma ordem lógica no gráfico
  dados <- dados %>%
    mutate(Metrica = factor(Metrica, levels = rev(unique(Metrica))))
  
  ggplot(dados, aes(x = Media, y = Metrica, color = Modelo)) +
    # Barras de erro (Intervalo de Confiança)
    geom_errorbarh(aes(xmin = Q025, xmax = Q975), 
                   height = 0.3, 
                   linewidth = 0.8,
                   position = position_dodge(width = 0.6)) +
    # Ponto da média com borda para destaque
    geom_point(size = 3.5, 
               position = position_dodge(width = 0.6)) +
    # Estética
    theme_minimal(base_size = 12) +
    scale_x_continuous(limits = c(0, 1), 
                       breaks = seq(0, 1, 0.1),
                       expand = c(0.02, 0)) +
    scale_color_brewer(palette = "Set1") + # Paleta de cores distinta
    labs(
      title = titulo,
      subtitle = "Modelos ajustados com limiares otimizados",
      x = "Valor da Métrica (Média e IC 95%)",
      y = NULL,
      color = "Modelo de Predição:"
    ) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey90"),
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold")
    )
}



gerar_tabela_estatistica <- function(dados, colunas_metricas = c("Accuracy", "F1", "Recall", "Precision")) {
  
  dados_resumidos <- dados %>%
    # Transforma de formato largo para longo baseado nas colunas escolhidas
    pivot_longer(cols = all_of(colunas_metricas), 
                 names_to = "Metrica", 
                 values_to = "Valor") %>%
    
    # Agrupa por Modelo (ou qualquer coluna identificadora) e pela Métrica
    group_by(Modelo, Metrica) %>%
    summarise(
      Min      = min(Valor, na.rm = TRUE),
      Q025     = quantile(Valor, probs = 0.025, na.rm = TRUE),
      Media    = mean(Valor, na.rm = TRUE),
      Mediana  = median(Valor, na.rm = TRUE),
      DP       = sd(Valor, na.rm = TRUE),
      Q975     = quantile(Valor, probs = 0.975, na.rm = TRUE),
      Max      = max(Valor, na.rm = TRUE),
      .groups  = 'drop'
    ) %>%
    
    # Arredondamento para facilitar a leitura
    mutate(across(where(is.numeric), ~ round(., 4)))
  
  return(dados_resumidos)
}






plot_deviance_residuals <- function(modelo, nome_modelo = "Modelo Reduzido") {
  
  # 1. Extrair os dados necessários
  # 'fitted' nos dá as probabilidades estimadas
  # 'resid' com type="deviance" nos dá os resíduos
  df_plot <- data.frame(
    probs = fitted(modelo),
    residuos = resid(modelo, type = "deviance")
  )
  
  # 2. Criar o gráfico
  ggplot(df_plot, aes(x = probs, y = residuos)) +
    # Adiciona uma linha suave (LOESS) para detectar tendências
    geom_smooth(method = "loess", color = "red", fill = "red", alpha = 0.2, linetype = "dashed") +
    # Adiciona os pontos (resíduos)
    geom_point(alpha = 0.5, color = "steelblue") +
    # Linha de referência no zero
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 1) +
    # Estética
    theme_minimal() +
    labs(
      title = paste("Deviance Residuals vs Estimated Probabilities"),
      subtitle = paste("Análise de diagnóstico:", nome_modelo),
      x = "Probabilidades Estimadas (Fitted Values)",
      y = "Resíduos Deviance"
    ) +
    theme(plot.title = element_text(face = "bold"))
}








analise_evolutiva_outliers <- function(modelo_orig, dados_treino, n_max = 10) {
  # 1. Calcular resíduos de Pearson para identificar a ordem de remoção
  res_p <- residuals(modelo_orig, type = "pearson")
  nomes_ordenados <- names(sort(abs(res_p), decreasing = TRUE))
  
  # Coeficientes originais para comparação
  coef_base <- coef(modelo_orig)
  
  # Lista para armazenar resultados
  resultados <- list()
  
  for (i in 1:n_max) {
    # Identificar os top i outliers
    outliers_remover <- nomes_ordenados[1:i]
    
    # Filtrar dados
    dados_temp <- dados_treino[!(rownames(dados_treino) %in% outliers_remover), ]
    
    # Reajustar o modelo (usando a mesma fórmula do original)
    mod_temp <- update(modelo_orig, data = dados_temp)
    coef_temp <- coef(mod_temp)
    
    # Calcular a Diferença Absoluta e Diferença Percentual
    dif_absoluta <- coef_temp - coef_base
    dif_relativa_pct <- (dif_absoluta / abs(coef_base)) * 100
    
    # Guardar resumo do impacto (média das mudanças dos coeficientes)
    resultados[[i]] <- data.frame(
      N_Removidos = i,
      Coeficiente = names(coef_temp),
      Valor_Original = as.numeric(coef_base),
      Novo_Valor = as.numeric(coef_temp),
      Diferenca_Abs = as.numeric(dif_absoluta),
      Variacao_Pct = as.numeric(dif_relativa_pct)
    )
  }
  
  return(bind_rows(resultados))
}
