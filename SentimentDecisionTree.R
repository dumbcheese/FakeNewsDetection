data <- read.csv("C:/Users/Admin/desktop/MLProjekatWorkingCode/sentiment.csv", stringsAsFactors = TRUE)
data <-na.omit(data)



str(data)

#normalization function in case we need it in the script
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#assigning the diagnosis as factors
data$label_fnn <- factor(data$label_fnn, levels = c('fake', 'real'),
                               labels = c("Fake", "Real"))


#unneeded normalization process
#data_n <- as.data.frame(lapply(data[2:31], normalize))
#str(data_n)


#dividing dataset
data_train <- data[1:500, 1:6]
data_test <- data[501:1048, 1:6]

str(data_train)

#defining seed and library
set.seed(123)
library(C50)

#
predModel <- C5.0(label_fnn ~ ., data_train)
predModelaaa <- predict(predModel, data_test)
summary(predModel)
predModel
plot(predModel)

#predModel <- C5.0(data_train[,-7], as.factor(data_train$label_fnn))
#summary(predModel)
#predModel
#plot(predModel)

data_pred <- predict(predModel, data_test)

library(gmodels)

CrossTable(data_test$label_fnn, data_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual', 'Predicted'))

