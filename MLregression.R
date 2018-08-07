#Simple Linear Regression Models
dataset <- read.csv("Salary_Data.csv")
set.seed(123)
index_train <- sample(1:nrow(dataset), 2/3 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

regressor <- lm(Salary ~ YearsExperience, data = training_set)

### predicting Test Set results
test_set$y_pred  <- predict(regressor, newdata = test_set)

### Visualizing the Training Sets
library(ggplot2)
ggplot() + 
  geom_point(aes(x = training_set$YearsExperience, y =  training_set$Salary),
             col = "red") + 
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            col = "blue") + 
  ggtitle("Salary vs Experience (Training Set)") + 
  xlab("Years of Experience") + ylab("Salary")




#Fitting Multiple Linear Regression to the training set
setwd("C:/Users/Denis Korsak/Desktop/Education/Udemy/Data Sets/Machine_Learning/Part 2 - Regression/Section 5 - Multiple Linear Regression/Multiple_Linear_Regression")
dataset <- read.csv("50_Startups.csv")
dataset$State <- factor(dataset$State, 
                        levels = c("New York","California","Florida"), 
                        labels = c(1,2,3))
set.seed(123)
index_train <- sample(1:nrow(dataset), 4/5 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

regressor <- lm(formula = Profit ~ .,
                data = training_set)
y_pred <- predict(regressor, newdata = test_set)


#Building the optimal model using Backward Elimination
regressor <- lm(formula = dataset$Profit ~ dataset$R.D.Spend + dataset$Administration
                +dataset$Marketing.Spend + dataset$State)


regressor <- lm(formula = dataset$Profit ~ dataset$R.D.Spend + dataset$Administration
                +dataset$Marketing.Spend)

regressor <- lm(formula = dataset$Profit ~ dataset$R.D.Spend + dataset$Marketing.Spend)

regressor <- lm(formula = dataset$Profit ~ dataset$R.D.Spend)


### Non-Linear Regression Models
#1. Polynomial Regression
setwd("C:/Users/Denis Korsak/Desktop/Education/Udemy/Data Sets/Machine_Learning/Part 2 - Regression/Section 6 - Polynomial Regression/Polynomial_Regression (2)/Polynomial_Regression")


dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[,c(2,3)]
# set.seed(123)
# index_train <- sample(1:nrow(dataset), 2/3 * nrow(dataset))
# training_set <- dataset[index_train,]
# test_set <- dataset[-index_train,]

##Just graph it

#Feature Scaling
# training_set[, 2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])


#Fitting Linear Regression Model
lin_reg <- lm(Salary~.,
              data = dataset)

#Fitting Polynomial Regression model
dataset$Level2 <- dataset$Level^2
dataset$Level3 <- dataset$Level^3
dataset$Level4 <- dataset$Level^4

poly_reg <- lm(formula = Salary ~.,
               data = dataset)


## Visualizing the Linear Regression

ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            col = "blue") + 
  ggtitle("Truth or Bluff") + xlab("Employment Levels") + ylab("Salary")


## Visualizing the Polynomial Regression

ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            col = "blue") + 
  ggtitle("Truth or Bluff") + xlab("Employment Levels") + ylab("Salary")

## Both

ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            col = "blue") + 
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            col = "black")
ggtitle("Truth or Bluff") + xlab("Employment Levels") + ylab("Salary")

## Predicting a new resulth with Linear Regression model
y_pred <- predict(lin_reg, newdata = data.frame(Level = 6.5))

## Predicting a new results with Polynomial Regression model
y_pred <- predict(poly_reg, newdata = data.frame(Level = 6.5,
                                                 Level2 = 6.5^2,
                                                 Level3 = 6.5^3,
                                                 Level4 = 6.5^4))
## SVR regression
dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[,c(2,3)]
library(e1071)
regressor <- svm(formula = Salary ~ .,
                 data = dataset,
                 type = "eps-regression")

#Predicting new resuls by SVR
y_pred <- predict(regressor, data.frame(Level = 6.5))

#Visualizing the SVR results
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            col = "blue") + 
  
  ggtitle("Truth or Bluff(SVR)") + xlab("Employment Levels") + ylab("Salary")


#Decision Tree Regression
dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[,c(2,3)]

#Fitting Decision Tree Regression
install.packages("rpart")
library(rpart)
regressor <- rpart(formula = Salary ~ . , data = dataset,
                   control = rpart.control(minsplit = 1))

#Predicting a new result
y_pred <- predict(regressor, data.frame(Level = 6.5))


#Graphing the results
x_grid <- seq(min(dataset$Level), max(dataset$Level),0.01)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             color = "red") +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            col = "blue") + 
  
  ggtitle("Truth or Bluff(SVR)") + xlab("Employment Levels") + ylab("Salary")


#Random Forest Regression
dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[,c(2,3)]

#Fitting Decision Tree Regression
library(rpart)
library(randomForest)
set.seed(1234)
regressor <- randomForest( x = dataset[1],
                           y = dataset$Salary,
                           ntree = 500)

#Predicting a new result
y_pred <- predict(regressor, data.frame(Level = 6.5))


#Graphing the results
x_grid <- seq(min(dataset$Level), max(dataset$Level),0.01)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary), 
             color = "red") +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            col = "blue") + 
  
  ggtitle("Truth or Bluff(SVR)") + xlab("Employment Levels") + ylab("Salary")

#Logistic Regression

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

#Running the Logistic regression model with Training Set
classifier <- glm(formula = Purchased ~ .,
                  family = binomial,
                  data = training_set)

#Predicting test set results with Logistic Regression
#type = to get the list of probabilities
prob_pred <- predict(classifier, type = "response", newdata = test_set[-3])
y_pred <- ifelse(prob_pred > 0.5,1,0)


#Building Confusion Matrix for the logistic Regression
cm <- table(test_set[,3],y_pred)

#Visualizing the logistic regression results
#install.packages("ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c("Age","EstimatedSalary")
prob_set <- predict(classifier, type = "response", newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5,1,0)
plot(set[,-3],
     main = "Logistic Regression(Training Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3","tomato"))
points(set, pch = 21, bg = ifelse(set[,3] == 1, "green4","red3"))


#Visualizing the logistic regression results
set = test_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1,X2)
colnames(grid_set) <- c("Age","EstimatedSalary")
prob_set <- predict(classifier, type = "response", newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5,1,0)
plot(set[,-3],
     main = "Logistic Regression(Test Set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1,X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = ".", col = ifelse(y_grid == 1, "springgreen3","tomato"))
points(set, pch = 21, bg = ifelse(set[,3] == 1, "green4","red3"))

