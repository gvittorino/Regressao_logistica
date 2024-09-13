# Instalar e carregar os pacotes necessários
# install.packages("caTools")
# install.packages("pROC")
# install.packages("openxlsx") : usado para exportar a matriz de correlação

library(caTools)
library(pROC)
library(openxlsx)

# Usar o conjunto de dados COLUNA VERTEBRAL
dados_reg <- read.table("dados_reg.txt", header = TRUE, sep = "\t")

# Matriz de correlação
matriz_correlacao <- cor(dados_reg[1:6])
print(matriz_correlacao)

# Exportar para Excel
write.xlsx(matriz_correlacao, "arquivo.xlsx")

# Transformando a variável classe em fator
dados_reg$classe <- as.factor(dados_reg$classe)

dados_reg$incidencia_pelvica <- NULL

# Definir a semente para reprodutibilidade
set.seed(123)

# Criar uma amostra de divisão
split <- sample.split(dados_reg$classe, SplitRatio = 0.75)

# Conjunto de treino
train_data <- subset(dados_reg, split == TRUE)

# Conjunto de teste
test_data <- subset(dados_reg, split == FALSE)

# Ajustar modelos de regressão logística "one-vs-all"
model_DH <- glm(I(classe == "DH") ~ ., data = train_data, family = binomial)
model_SL <- glm(I(classe == "SL") ~ ., data = train_data, family = binomial)
model_NO <- glm(I(classe == "NO") ~ ., data = train_data, family = binomial)

# Prever probabilidades para o conjunto de teste
prob_DH <- predict(model_DH, newdata = test_data, type = "response")
prob_SL <- predict(model_SL, newdata = test_data, type = "response")
prob_NO <- predict(model_NO, newdata = test_data, type = "response")

# Criar uma matriz de probabilidades preditas
prob_pred <- data.frame(DH = prob_DH, SL = prob_SL, NO = prob_NO)

# Inicializar uma lista para armazenar os objetos ROC
roc_list <- list()

# Calcular e plotar a curva ROC para cada classe
for (i in 1:3) {
  # Criar a variável de resposta binária (one-vs-all)
  binary_response <- as.numeric(test_data$classe == levels(test_data$classe)[i])
  
  # Calcular a curva ROC
  roc_obj <- roc(binary_response, prob_pred[, i])
  roc_list[[i]] <- roc_obj
  
  # Plotar a curva ROC
  if (i == 1) {
    plot(roc_obj, main = "Curvas ROC para cada classe", col = i)
  } else {
    plot(roc_obj, add = TRUE, col = i)
  }
}

# Adicionar a legenda
legend("bottomright", legend = levels(test_data$classe), col = 1:3, lty = 1)


summary(model_DH)
summary(model_SL)
summary(model_NO)

# NOVOS MODELOS - removendo os coeficientes não significativos a 10%
new_model_DH <- glm(I(classe == "DH") ~ ., data = train_data, family = binomial)
new_model_SL <- glm(I(classe == "SL") ~ ., data = train_data, family = binomial)
new_model_NO <- glm(I(classe == "NO") ~ ., data = train_data, family = binomial)

# Atribuir a classe com a maior probabilidade predita
predicted_classes <- colnames(prob_pred)[max.col(prob_pred, ties.method = "first")]


# Criar a matriz de confusão teste
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$classe)
print(confusion_matrix)


# Calcular a acurácia
accuracy <- mean(predicted_classes == test_data$classe)
print(paste("Acurácia do modelo:", accuracy))
