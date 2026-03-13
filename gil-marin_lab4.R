##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
# setwd("C:/Users/gilmaj2/OneDrive - Rensselaer Polytechnic Institute/02-phd-courses/2026S_MGMT6600_DataAnalytics/labs/04")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))




# PCA ----

## Prepare data

X <- wine[,-1]

Y <- wine$Type

## Convert to a matrix and center the data
X_mat <- as.matrix(X)
X_centered <- scale(X_mat, center = TRUE, scale = TRUE)

## PCs
pca_result <- princomp(X_centered)
pca_result$loadings
autoplot(pca_result, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)

# variables that contribute the most to the 1st PC: Flavanoids and Total phenols 

summary(pca_result)
plot(pca_result, type = "l")



# Classifier 1 (Using three variables) ----
selected_vars <- c("Ash", "Magnesium", "Hue")

## Split train/test
set.seed(42)
train_index <- sample(nrow(wine), 0.7*nrow(wine))
train <- wine[train_index,]
test <- wine[-train_index,]

## Separate features (X) and labels (Y)
X1_train <- train[, selected_vars]
Y1_train <- train$Type
X1_test <- test[, selected_vars]
Y1_test <- test$Type

## Fit kNN model
# define a function to perform kNN and calculate accuracy for a given k
cross_val_knn1 <- function(k) {
  # fit the model
  knn.pred <- class::knn(train=X1_train, test=X1_test, cl=Y1_train, k=k)
  # compute accuracy
  accuracy <- mean(knn.pred == Y1_test)
  
  return(accuracy)
}

k_values <- seq(from=10, to=100,  length.out=10)

for (k in k_values) {
  cv.results <- cross_val_knn1(k)
  print(paste("k =", k, ". Accuracy =", round(cv.results, 3)))
}

# Optimal K is 10 with accuracy of 0.648

knn1 <- class::knn(train=X1_train, test=X1_test, cl=Y1_train, k=10)



# Classifier 2 (Using first two PCs) ----
## Get the first two PCs
X2 <- pca_result$scores
X2_train <- X2[train_index, 1:2]
X2_test <- X2[-train_index, 1:2]
Y2_train <- train$Type
Y2_test <- test$Type

## Fit kNN model
cross_val_knn2 <- function(k) {
  # fit the model
  knn.pred <- class::knn(train=X2_train, test=X2_test, cl=Y2_train, k=k)
  # compute accuracy
  accuracy <- mean(knn.pred == Y1_test)
  
  return(accuracy)
}

for (k in k_values) {
  cv.results <- cross_val_knn2(k)
  print(paste("k =", k, ". Accuracy =", round(cv.results, 3)))
}

# Optimal K is 10 with accuracy of 0.981

knn2 <- class::knn(train=X2_train, test=X2_test, cl=Y2_train, k=10)



## Compare both classifiers ----

# Confusion matrix for Classifier 1
cm1 <- as.matrix(table(knn1, Y1_test, dnn=list('predicted','actual')))
print(cm1)

# Confusion matrix for Classifier 2
cm2 <- as.matrix(table(knn2, Y2_test, dnn=list('predicted','actual')))
print(cm2)

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


# Compute metrics
metrics1 <- compute_metrics(cm1)
metrics2 <- compute_metrics(cm2)

print(paste("Classifier 1 - Accuracy:", round(metrics1$accuracy, 3)))
print(paste("Classifier 2 - Accuracy:", round(metrics2$accuracy, 3)))

print(paste("Classifier 1 - Precision:", round(metrics1$precision, 3)))
print(paste("Classifier 2 - Precision:", round(metrics2$precision, 3)))

print(paste("Classifier 1 - Recall:", round(metrics1$recall, 3)))
print(paste("Classifier 2 - Recall:", round(metrics2$recall, 3)))

print(paste("Classifier 1 - F1 Score:", round(metrics1$f1_score, 3)))
print(paste("Classifier 2 - F1 Score:", round(metrics2$f1_score, 3)))

