
# Pegando dados
data= read.csv(here::here("data", "heart_cleveland_upload.csv"))

head(data)

# Transformando variáveis categóricas em fatores
cols_para_fator <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal")
data[cols_para_fator] <- lapply(data[cols_para_fator], as.factor)

data$condition <- factor(data$condition,
                         levels = c(0, 1),
                         labels = c("Saudavel", "Doente"))
