library(rpart)
library(ggplot2)
library(randomForest)
library(caTools)
library(rpart.plot)

bank_dataset <- read.csv('bank.csv', stringsAsFactors = TRUE)

# EDA
str(bank_dataset)
head(bank_dataset)

# Splitting dataset
set.seed(123)
train_dataset_ids <- sample(seq_len(nrow(bank_dataset)), size = floor(0.70 * nrow(bank_dataset)))

train_dataset <- bank_dataset[train_dataset_ids, ]
test_dataset <- bank_dataset[-train_dataset_ids, ]

# Decision Tree
dt_model <- rpart(y ~ .,method = "class", data = train_dataset)
predictions <- predict(dt_model, test_dataset,type='class')
confusionMarix <- table(test_dataset$y,predictions)
accuracy <- sum(diag(confusionMarix)) / sum(confusionMarix)
print(paste0("Accuracy: ", format(round(accuracy*100, 2), nsmall = 2),'%'))
rpart.plot(dt_model,type=1)
dt_model

# Random Forest
rf_model <- randomForest(y ~ ., data = train_dataset, mtry = 3,importance = TRUE, na.action = na.omit)
predictions <- predict(rf_model, test_dataset)
confusionMarix <- table(test_dataset$y,predictions)
accuracy <- sum(diag(confusionMarix)) / sum(confusionMarix)
print(paste0("Accuracy: ", format(round(accuracy*100, 2), nsmall = 2),'%'))
rf_model

# Logistc Regression
logistic_model <- glm(y ~ ., data = train_dataset, family =  "binomial")
predictions <- predict(logistic_model, test_dataset, type = "response")
confusionMarix <- table(test_dataset$y,predictions >= 0.5)
accuracy <- sum(diag(confusionMarix)) / sum(confusionMarix)
print(paste0("Accuracy: ", format(round(accuracy*100, 2), nsmall = 2),'%'))
summary(logistic_model)
