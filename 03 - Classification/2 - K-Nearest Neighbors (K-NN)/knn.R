# K-NN Classification

# Import the dataset
dataset <- read.csv('Social_Network_Ads.csv')
dataset <- dataset[,3:5]

# Split dataset into train and test dataset
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training.set <- subset(dataset, split == TRUE)
test.set <- subset(dataset, split == FALSE)

# Feature Scaling
training.set[-3] <- scale(training.set[-3])
test.set[-3] <- scale(test.set[-3])

# Fitting K-NN to the Training set and predicting Test set results
library(class)
y.pred <- knn(train = training.set[, -3],
              test = test.set[, -3],
              cl = training.set[, 3],
              k = 5)

# Confusion Matrix
cm <- table(test.set[, 3], y.pred)
accuracy <- (cm[1, 1] + cm[2, 2]) / dim(test.set)[1]

# Visualising the Training set results
library(ElemStatLearn)
X1 <- seq(min(training.set[1]) - 1, max(training.set[1]) + 1, by = 0.01)
X2 <- seq(min(training.set[2]) - 1, max(training.set[2]) + 1, by = 0.01)
grid.set <- expand.grid(X1, X2)
colnames(grid.set) <- c('Age', 'EstimatedSalary')
z.grid <- knn(train = training.set[, -3],
              test = grid.set[, -3],
              cl = training.set[, 3],
              k = 5)
plot(training.set[-3],
     main = 'K-NN (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(z.grid), length(X1), length(X2)), add = TRUE)
points(grid.set, pch = '.', col = ifelse(z.grid == 1, 'springgreen3', 'tomato'))
points(training.set, pch = 21, bg = ifelse(training.set[3] == 1, 'green4', 'red3'))

# Visualizing the Test set results
X1 <- seq(min(test.set[1]) - 1, max(test.set[1]) + 1, by = 0.01)
X2 <- seq(min(test.set[2]) - 1, max(test.set[2]) + 1, by = 0.01)
grid.set <- expand.grid(X1, X2)
colnames(grid.set) <- c('Age', 'EstimatedSalary')
z.grid <- knn(train = training.set[, -3],
              test = grid.set[, -3],
              cl = training.set[, 3],
              k = 5)
plot(test.set[-3],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(z.grid), length(X1), length(X2)), add = TRUE)
points(grid.set, pch = '.', col = ifelse(z.grid == 1, 'springgreen3', 'tomato'))
points(test.set, pch = 21, bg = ifelse(test.set[3] == 1, 'green4', 'red3'))