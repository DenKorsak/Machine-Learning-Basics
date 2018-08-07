
#For those who interested in the original datasets, please do not hesitate to contact me :) 




#K-Nearest Neighbours(K-NN)

#Data Preprocessing
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[,c(3:5)]
set.seed(123)
index_train <- sample(1:nrow(dataset), 3/4 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

#Feature Scaling
training_set[, 1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])

#Running the K-NN model to the Training Set and PRedicting Test set results
#install.packages("class")
library(class)
y_pred <- knn(training_set[,-3], 
              test_set[,-3], 
              cl = training_set[,3],
              k = 5) 


#Building Confusion Matrix for the logistic Regression
cm <- table(test_set[,3],y_pred)

#Visualizing the K-NN regression results
set = training_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c("Age","EstimatedSalary")
y_grid <- knn(training_set[,-3], 
              grid_set, 
              cl = training_set[,3],
              k = 5) 
plot(set[,-3],
     main = "K-NN(Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3","tomato"))
points(set, pch = 21, bg = ifelse(set[,3] == 1, "green4","red3"))

#K-NN results for TEST SET

set = test_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c("Age","EstimatedSalary")
y_grid <-  knn(training_set[,-3], 
               grid_set, 
               cl = training_set[,3],
               k = 5) 
plot(set[,-3],
     main = "K-NN(Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3","tomato"))
points(set, pch = 21, bg = ifelse(set[,3] == 1, "green4","red3"))



#Support Vector Machines

#Data Preprocessing
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[,c(3:5)]
set.seed(123)
index_train <- sample(1:nrow(dataset), 3/4 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

#Feature Scaling
training_set[, 1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])

#Running the SVM model to the Training Set and PRedicting Test set results
library(e1071)
#Running the Logistic regression model with Training Set
classifier <- svm(formula = Purchased ~ .,data = training_set, type = "C-classification", kernel = "linear")

#Predicting test set results with Logistic Regression
#type = to get the list of probabilities
y_pred <- predict(classifier, type = "response", newdata = test_set[-3])

#Building Confusion Matrix for the SVM Regression
cm <- table(test_set[,3],y_pred)

#Visualizing the SVM regression results
set = training_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c("Age","EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = "K-NN(Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3","tomato"))
points(set, pch = 21, bg = ifelse(set[,3] == 1, "green4","red3"))

#SVM results for TEST SET
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



#Naive Bayes Method
#Data Preprocessing
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[,c(3:5)]
#Encoding the target feature as factor
dataset$Purchased <- factor(dataset$Purchased, levels = c(0,1))
set.seed(123)
index_train <- sample(1:nrow(dataset), 3/4 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

#Feature Scaling
training_set[, 1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])


#Running the Naive Bayes model to the Training Set and predicting test set results
library(e1071)

#Running the Naive Bayes regression model with Training Set
classifier <- naiveBayes(x = training_set[-3], y = training_set$Purchased)

#Predicting test set results with Naive Bayes Classifier
y_pred <- predict(classifier, newdata = test_set[,-3])

#Building Confusion Matrix for the Naive Bayes Regression
cm <- table(test_set[,3],y_pred)

#Visualizing the Naive Bayes model results
set = training_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c("Age","EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = "NaiveBayes(Training Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3","tomato"))
points(set, pch = 21, bg = ifelse(set[,3] == 1, "green4","red3"))

#Naive Bayes results for the test set
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'NaiveBayes (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


#Decision Tree Classification 
#Plotting Decision Trees
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[,c(3:5)]
dataset$Purchased <- factor(dataset$Purchased, levels = c(0,1))
set.seed(123)
index_train <- sample(1:nrow(dataset), 3/4 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

classifier <- rpart(formula = Purchased ~ ., data = training_set)
y_pred <- predict(classifier, newdata = test_set[,-3], type = "class")

plot(classifier)
text(classifier)


#Data Preprocessing
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[,c(3:5)]
#Encoding the target feature as factor
dataset$Purchased <- factor(dataset$Purchased, levels = c(0,1))
set.seed(123)
index_train <- sample(1:nrow(dataset), 3/4 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

#Feature Scaling
training_set[, 1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])


#Running the Decision Tree model to the Training Set and predicting test set results
library(e1071)
library(rpart)

#Running the Decision Tree regression model with Training Set
classifier <- rpart(formula = Purchased ~ ., data = training_set)

#Predicting test set results with Decision Tree Classifier
y_pred <- predict(classifier, newdata = test_set[,-3], type = "class")

#Building Confusion Matrix for the Decision Tree Regression
cm <- table(test_set[,3],y_pred)

#Visualizing the Decision Tree results
set = training_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c("Age","EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set, type = "class")
plot(set[,-3],
     main = "Decision Tree(Training Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3","tomato"))
points(set, pch = 21, bg = ifelse(set[,3] == 1, "green4","red3"))

#Naive Bayes results for the test set
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


#Random Forest Classification

#Plotting Decision Trees
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[,c(3:5)]
dataset$Purchased <- factor(dataset$Purchased, levels = c(0,1))
set.seed(123)
index_train <- sample(1:nrow(dataset), 3/4 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

classifier <- rpart(formula = Purchased ~ ., data = training_set)
y_pred <- predict(classifier, newdata = test_set[,-3], type = "class")

plot(classifier)
text(classifier)

#Random Forest Classification

#Data Preprocessing
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[,c(3:5)]
#Encoding the target feature as factor
dataset$Purchased <- factor(dataset$Purchased, levels = c(0,1))
set.seed(123)
index_train <- sample(1:nrow(dataset), 3/4 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

#Feature Scaling
training_set[, 1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])


#Running the Random Forest Classification to the Training Set and predicting test set results
library(e1071)
library(rpart)
library(randomForest)

#Running the Random Forest Classification regression model with Training Set
classifier <- randomForest(x = training_set[,-3], y = training_set[,3], ntree = 10)

#Predicting test set results with Random Forest Classification
y_pred <- predict(classifier, newdata = test_set[,-3], type = "class")

#Building Confusion Matrix for the Random Forest Classification
cm <- table(test_set[,3],y_pred)

#Visualizing the Random Forest Classification results
set = training_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c("Age","EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set, type = "class")
plot(set[,-3],
     main = "Random Forest(Training Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3","tomato"))
points(set, pch = 21, bg = ifelse(set[,3] == 1, "green4","red3"))

#Random Forest Classification for the test set
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Random Forest(Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


## K-Means Clustering

data <- read.csv("mall.csv")
X <- data[,c(4,5)]

#Using the elbow method to find the optimum number of clusters
set.seed(6)
wcss <- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(X,i)$withinss)
plot(1:10,wcss, type = "b", main = paste("Clusters of clients"), xlab = "Number of clients", ylab = "WCSS")

#Applying K-means to the mall dataset
set.seed(29)
kmeans <- kmeans(X,5,iter.max = 300,nstart = 10)

#Visualizing the clusters
#install.packages("cluster")
library(cluster)
clusplot(X, kmeans$cluster,
         lines = 0,
         color = T,
         shade = T,
         labels = 2,
         plotchar = F,
         span = T)


#Hierarchical Clustering
read.csv("Mall_Customers.csv") -> dataset
X <- dataset[,c(4,5)]

#Using the dendrogram to find the optimal number of clusters
dendrogram <- hclust(dist(X,method = "euclidean"), method = 'ward.D')
plot(x = dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean Distance')



#Fitting  hierarchical clustering to the mall dataset
hc <- hclust(dist(X,method = "euclidean"), method = 'ward.D')
y_hc <- cutree(hc,k = 5)

#Visualizing our clusters using clusplot
clusplot(X, y_hc,
         lines = 0,
         color = T,
         shade = T,
         labels = 2,
         plotchar = F,
         span = T)
