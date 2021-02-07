data <- read.csv("fnn_testExclAndQuest.csv", stringsAsFactors = FALSE)
data <- na.omit(data);
data <- data[-1]
data <- data[-1]
data <- data[-1]
data <- data[-1]
data <- data[-1]
data <- data[-1]
data <- data[-1]
data <- data[-3]



data$label_fnn <- factor(data$label_fnn, levels = c('fake', 'real'),
                            labels = c("Fake", "Real"))

data$label_fnn <- as.numeric(data$label_fnn) 
str(data)
#normalizing is an essential part of the data preparation process
#we could have also used a (worst case - mean)/(standard deviation) to normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#dividing the data 80:20
data_train <- data[1:400, 1:3]
data_test <- data[401:1000, 1:3]

data_train_labels <- data[1:400, 3]
data_test_labels <- data[401:1000, 3]


library(class)

data_pred <- knn(train = data_train, test = data_test,
                       cl = data_train_labels, k = 7)

data_pred

library(gmodels)

CrossTable(x = data_test_labels, y = data_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual', 'Predicted'))

