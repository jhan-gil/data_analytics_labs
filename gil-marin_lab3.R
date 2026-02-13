## Lab 3

rm(list=ls())
gc()

library(class)
library(cluster)
library(factoextra)

# Set working directory
setwd("C:/Users/gilmaj2/Downloads/03")


# Exercise 1 ----

## read data
abalone <- read.csv("./abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c(
  "sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght',
  'viscera_wieght', 'shell_weight', 'rings'
)

## derive age group based in number of rings
abalone$age.group <- cut(
  abalone$rings,
  br=c(0,8,11,35),
  labels = c("young", 'adult', 'old')
)

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]



## Model 1: using all weight features

## separate x (features) & y (class labels)
X_train <- train[,4:7]
Y_train <- train[,8]
X_test <- test[,4:7]

# fite the model
knn.pred1 <- class::knn(train=X_train, test=X_test, cl=Y_train, k=5)
# contingency table
table(knn.pred1, test$age.group, dnn=list('predicted','actual'))



## Model 2: using only length, diameter, and height

## separate x (features) & y (class labels)
X_train <- train[,1:3]
Y_train <- train[,8]
X_test <- test[,1:3]

# fite the model
knn.pred2 <- class::knn(train=X_train, test=X_test, cl=Y_train, k=5)
# contingency table
table(knn.pred2, test$age.group, dnn=list('predicted','actual'))


## Select k using cross-validation

# model 1 was better
X_train <- train[,4:7]
Y_train <- train[,8]
X_test <- test[,4:7]

# list of k values to try
k.values <- seq(1, 20, by=1)

# define a function to perform kNN and calculate accuracy for a given k
cross_val_knn <- function(k) {
  # fit the model
  knn.pred <- class::knn(train=X_train, test=X_test, cl=Y_train, k=k)
  # compute accuracy
  accuracy <- mean(knn.pred == test$age.group)

  return(accuracy)
}

for (k in k.values) {
  cv.results <- cross_val_knn(k)
  print(paste("k =", k, ". Accuracy =", round(cv.results, 3)))
}

# optimal k is 17 with accuracy of 0.622




## Exercise 2 ----

# k-means clustering

# list of k values to try
k.values <- seq(2, 10, by=1)

# function to find the optimal k usig the wcss
wcss_kmeans <- function(k) {
  kmeans.result <- kmeans(X_train, centers=k, nstart=25)
  return(kmeans.result$tot.withinss)
}

# find wcss for each k
for (k in k.values) {
  wcss <- wcss_kmeans(k)
  print(paste("k =", k, ". WCSS =", round(wcss, 3)))
}

# optimal k (read from the print statements) is 3
opt_k <- 7

# plot with optimal k
silhouette.result <- cluster::silhouette(
  kmeans(X_train, centers=opt_k, nstart=25)$cluster,
  dist(X_train)
)
fviz_silhouette(silhouette.result)


# PAM clustering

# function to find the optimal k using sum of dissimilarities (sum of distances)
diss_kpam <- function(k) {
  pam.result <- cluster::pam(X_train, k=k, nstart=25)
  sumdiss <- pam.result$objective[2]
  return(sumdiss)
}

for (k in k.values) {
  diss <- diss_kpam(k)
  print(paste("k =", k, ". Sum of dissimilarities =", round(diss, 3)))
}

# find optimal k (read from the print statements) is 3
opt_k <- 6

# plot with optimal k
silhouette.result <- cluster::silhouette(
  cluster::pam(X_train, k=opt_k, nstart=25)$cluster,
  dist(X_train)
)
fviz_silhouette(silhouette.result)
