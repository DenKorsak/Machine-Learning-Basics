
#Contact me in person for the datasets! :) 

#Apriori Machine Learning Algorithm
setwd("C:/Users/Denis Korsak/Desktop/Education/Udemy/Data Sets/Machine_Learning/Part 5 - Association Rule Learning/Section 28 - Apriori/Apriori-R/Apriori")
read.csv('Market_Basket_Optimisation.csv', header = FALSE) -> dataset

#To create the sparse matrix
dataset <- read.transactions('Market_Basket_Optimisation.csv', sep = ',',
                             rm.duplicates = TRUE)
#Data Preprocessing
#install.packages('arules')
library(arules)
itemFrequencyPlot(dataset,topN = 100)

#Training Apriori on the dataset
rules <- apriori(dataset, parameter = list(support = 0.004 , confidence = 0.2))

#Visualising the results
inspect(sort(rules, by = 'lift')[1:10])



#Eclat Model - apriori model simplified
read.csv('Market_Basket_Optimisation.csv', header = FALSE) -> dataset

#To create the sparse matrix
dataset <- read.transactions('Market_Basket_Optimisation.csv', sep = ',',
                             rm.duplicates = TRUE)

#Data Preprocessing
#install.packages('arules')
library(arules)
itemFrequencyPlot(dataset,topN = 100)

#Training Eclat on the dataset
rules <- eclat(dataset, parameter = list(support = 0.004,minlen = 2))
#minlen = just 2 items bought together

#Visualising the results
inspect(sort(rules, by = 'support')[1:10])


#Reinforcement Learning
#Upper Confidence Bound

#Importing Data Set
setwd("C:/Users/Denis Korsak/Desktop/Education/Udemy/Data Sets/Machine_Learning/Part 6 - Reinforcement Learning/Section 32 - Upper Confidence Bound (UCB)")
dataset <- read.csv("Ads_CTR_Optimisation.csv")


# Upper Confidence Bound
# Importing the dataset
dataset = read.csv('Ads_CTR_Optimisation.csv')

# Implementing UCB
N = 10000
d = 10
ads_selected = integer(0)
numbers_of_selections = integer(d)
sums_of_rewards = integer(d)
total_reward = 0
for (n in 1:N) {
  ad = 0
  max_upper_bound = 0
  for (i in 1:d) {
    if (numbers_of_selections[i] > 0) {
      average_reward = sums_of_rewards[i] / numbers_of_selections[i]
      delta_i = sqrt(3/2 * log(n) / numbers_of_selections[i])
      upper_bound = average_reward + delta_i
    } else {
      upper_bound = 1e400
    }
    if (upper_bound > max_upper_bound) {
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  numbers_of_selections[ad] = numbers_of_selections[ad] + 1
  reward = dataset[n, ad]
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward
  total_reward = total_reward + reward
}

# Visualising the results
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')




#Thompson Sampling Problem

#Importing Data Set
dataset <- read.csv("Ads_CTR_Optimisation.csv")

# Upper Confidence Bound

# Importing the dataset
dataset = read.csv('Ads_CTR_Optimisation.csv')

# Implementing the algorithm
N = 10000
d = 10
ads_selected = integer(0)
numbers_of_rewards_1 <- integer(d)  
numbers_of_rewards_0 <- integer(d)
total_reward = 0
for (n in 1:N) {
  ad = 0
  max_random = 0
  for (i in 1:d) {
    random_beta = rbeta(n = 1,
                        shape1 = numbers_of_rewards_1[i]+1,
                        shape2 = numbers_of_rewards_0[i]+1)
    if (random_beta > max_random) {
      max_random = random_beta
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  reward = dataset[n, ad]
  if (reward == 1) {
    numbers_of_rewards_1[ad]= numbers_of_rewards_1[ad] + 1
  } else {
    numbers_of_rewards_0[ad]= numbers_of_rewards_0[ad] + 1
  }
  total_reward = total_reward + reward
}

# Visualising the results
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')



#Deep Learning
#Artifical Neural Network

read.csv("Churn_Modelling.csv") -> dataset
dataset = dataset[4:14]

# Encoding the categorical variables as factors
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
index_train <- sample(1:nrow(dataset), 4/5 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

#Feature Scaling
training_set[,-11] <- scale(training_set[,-11])
test_set[,-11] <- scale(test_set[,-11])

##Building an Artificial Neural Network to the training set

#install.packages(Neuralnet, Nnet, deepnet)
#install.packages("h2o")
library(h2o)
h2o.init(nthreads = -1)
classifier = h2o.deeplearning(y = "Exited",
                              training_frame = as.h2o(training_set),
                              activation = "Rectifier",
                              hidden = c(6,6),
                              epochs = 100,
                              train_samples_per_iteration = -2)

#Predicting the Test set results
prob_pred = h2o.predict(classifier, newdata = as.h2o(test_set[-11]))
y_pred = ifelse(prob_pred>0.5,1,0)

#Confusion Matrix
cm = table(test_set[,11], as.vector(y_pred))



##Dimensionality reduction - PCA and then using Logistic Regression
dataset <- read.csv('Wine.csv')

set.seed(123)
index_train <- sample(1:nrow(dataset), 4/5 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

#Feature Scaling
training_set[-14] <- scale(training_set[-14])
test_set[-14] <- scale(test_set[-14])

#Applying PCA
#install.packages("caret")
library(caret)
library(e1071)

pca = preProcess(x = training_set[-14],method = "pca", pcaComp = 2)
training_set = predict(pca, training_set)
training_set = training_set[c(2,3,1)]
test_set = predict(pca, test_set)
test_set = test_set[c(2,3,1)]

#Implimenting LDA (linear Discriminant Analysis)
dataset <- read.csv('Wine.csv')
set.seed(123)
index_train <- sample(1:nrow(dataset), 4/5 * nrow(dataset))
training_set <- dataset[index_train,]
test_set <- dataset[-index_train,]

#Feature Scaling
training_set[-14] <- scale(training_set[-14])
test_set[-14] <- scale(test_set[-14])

#Applying LDA
library(MASS)
lda = lda(formula = Customer_Segment ~ ., data = training_set)
training_set = as.data.frame(predict(lda,training_set))
training_set = training_set[c(5,6,1)]
test_set = as.data.frame(predict(lda,test_set))
test_set = test_set[c(5,6,1)]

