# Lab 5

# Load packages ----
library(ggplot2)
library(readr)
library(dplyr)
library(randomForest)
library(e1071)

# Load data ----
df_wine <- readr::read_csv("labs/05/wine.data", col_names = FALSE)
names(df_wine) <- c(
    "Type","Alcohol","MalicAcid","Ash","AlcalinityOfAsh","Magnesium",
    "TotalPhenols","Flavanoids","NonflavanoidPhenols","Proanthocyanins",
    "ColorIntensity","Hue","Od280/od315OfDilutedWines","Proline"
)
df_wine <- df_wine %>% dplyr::mutate(Type = as.factor(Type))

# Scale values
df_scaled <- df_wine %>%
    mutate(across(where(is.numeric), \(x) scale(x, center = TRUE, scale = TRUE)))


# SVM Classifiers ----

## SVM 1 - Linear Kernel

# Split train/test
N <- nrow(df_wine)
set.seed(42)
train.indexes <- sample(N,0.7*N)

df_train <- df_scaled[train.indexes,]
df_test <- df_scaled[-train.indexes,]

# Feature plots
(
    ggplot(df_train, aes(x = Flavanoids, y = TotalPhenols, colour = Type))
    + geom_point()
    + theme_bw()
)

(
    ggplot(df_train, aes(x = Ash, y = Hue, colour = Type))
    + geom_point()
    + theme_bw()
)

(
    ggplot(df_train, aes(x = Flavanoids, y = MalicAcid, colour = Type))
    + geom_point()
    + theme_bw()
)


# Train SVM model
gamma_vals <- seq(0.1, 10, .1)
C_vals <- seq(1, 20, 1)

tune_svm1 <- e1071::tune.svm(
    Type ~ Flavanoids + TotalPhenols + MalicAcid,
    data = df_train,
    kernel = 'linear',
    gamma = gamma_vals,
    cost = C_vals
)

gamma <- tune_svm1$best.parameters$gamma
C <- tune_svm1$best.parameters$cost

# Predict with test data
pred1 <- predict(svm_1, df_test)

# Confusion matrix
cm1 <- as.matrix(table(pred1, df_test$Type, dnn=list('predicted','actual')))
print(cm1)

# Function to compute accuracy, precision, recall, and F1-score
compute_metrics <- function(cm) {
    # Number of instances
    n <- sum(cm)
    # Number of classes
    nc <- nrow(cm)
    # Number of correctly classified instances per class
    diag <- diag(cm)
    # Number of instances per class
    rowsums <- apply(cm, 1, sum)
    # Number of predictions per class
    colsums <- apply(cm, 2, sum)
    
    # Accuracy
    accuracy <- sum(diag) / n
    # Precision
    precision <- diag / colsums
    # Recall
    recall <- diag / rowsums
    # F1-score
    f1_score <- 2 * (precision * recall) / (precision + recall)
    
    return(list(accuracy=accuracy, precision=precision, recall=recall, f1_score=f1_score))
}

met_svm1 <- compute_metrics(cm1)
print(met_svm1)


## SVM 2 - Radia Kernel

# Train SVM model
tune_svm2 <- e1071::tune.svm(
    Type ~ Flavanoids + TotalPhenols + MalicAcid,
    data = df_train,
    kernel = 'radial',
    gamma = gamma_vals,
    cost = C_vals
)

gamma <- tune_svm2$best.parameters$gamma
C <- tune_svm2$best.parameters$cost

svm2 <- e1071::svm(
    Type ~ Flavanoids + TotalPhenols + MalicAcid,
    data = df_train,
    kernel = 'radial',
    gamma = gamma,
    cost = C
)


# Predict with test data
pred2 <- predict(svm2, df_test)

# Confusion matrix
cm2 <- as.matrix(table(pred2, df_test$Type, dnn=list('predicted','actual')))
print(cm2)

met_svm2 <- compute_metrics(cm2)
print(met_svm2)


# Random Foest Classifiers ----

rf3 <- randomForest::randomForest(
    Type ~ Flavanoids + TotalPhenols + MalicAcid,
    data = df_train
)

# Predict with test data
pred3 <- predict(rf3, df_test)

# Confusion matrix
cm3 <- as.matrix(table(pred3, df_test$Type, dnn=list('predicted','actual')))
print(cm3)

met_rf3 <- compute_metrics(cm3)
print(met_rf3)


# Compare metrics ----
print(paste("SVM 1 - Accuracy:", round(met_svm1$accuracy, 3)))
print(paste("SVM 2 - Accuracy:", round(met_svm2$accuracy, 3)))
print(paste("RF - Accuracy:", round(met_rf3$accuracy, 3)))

print(paste("SVM 1 - Precision:", round(met_svm1$precision, 3)))
print(paste("SV 2 - Precision:", round(met_svm2$precision, 3)))
print(paste("RF - Precision:", round(met_rf3$precision, 3)))

print(paste("SVM 1 - Recall:", round(met_svm1$recall, 3)))
print(paste("SV 2 - Recall:", round(met_svm2$recall, 3)))
print(paste("RF - Recall:", round(met_rf3$recall, 3)))

print(paste("SVM 1 - F1 Score:", round(met_svm1$f1_score, 3)))
print(paste("SV 2 - F1 Score:", round(met_svm2$f1_score, 3)))
print(paste("RF - F1 Score:", round(met_rf3$f1_score, 3)))
