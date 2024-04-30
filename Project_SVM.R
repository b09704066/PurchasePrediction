data = read.csv('/Users/nina/Downloads/online_shoppers_intention.csv')
data <- na.omit(data)
data <- as.data.frame(data)


#one-hot encoeing
library(caret)
library(mltools)
library(data.table)

#dummy <- dummyVars(Revenue ~., data=data)
#newdata <- data.frame(predict(dummy, newdata = data))
newdata <- one_hot(as.data.table(data))

#data <- model.matrix(~ Weekend - 1, data = data)
#data <- model.matrix(~ TrafficType - 1, data = data)
#data <- model.matrix(~ OperatingSystems - 1, data = data)
#data <- model.matrix(~ Region - 1, data = data)
#data <- model.matrix(~ Browser - 1, data = data)
#data <- model.matrix(~ Month - 1, data = data)
#data <- model.matrix(~ VisitorType - 1, data = data)

# convert categorical data to factor
##data$Weekend<- as.factor(data$Weekend)
#data$TrafficType <- as.factor(data$TrafficType)
#data$OperatingSystems <- as.factor(data$OperatingSystems)
#data$Region <- as.factor(data$Region)
#data$Browser <- as.factor(data$Browser)
#data$Month <- as.factor(data$Month)
#data$VisitorType <- as.factor(data$VisitorType)

# Encoding the target feature as factor 
#newdata$Revenue = factor(newdata$Revenue, levels = c(0, 1)) 


#Train-test split
index <- sample(nrow(data),nrow(data)*0.80)
train = newdata[index,]
test = newdata[-index,]

#Support Vector Machine (SVM)
library(e1071)
positiveWeight = 1.0 / (nrow(subset(train, Revenue == TRUE)) / nrow(train))
negativeWeight = 1.0 / (nrow(subset(train, Revenue != TRUE)) / nrow(train))
#class_Weights <- ifelse(train$Revenue== TRUE, positiveWeight, negativeWeight)
classifier = svm(formula = Revenue ~ ., data = train, type = 'C-classification', 
                 class.weights = list("TRUE" = positiveWeight, "FALSE" = negativeWeight), 
                 kernel = 'linear', scale = TRUE, probability = TRUE) 

#in-sample performance
y_train_pred = predict(classifier, newdata = train[, 1:17])
table(train$Revenue, y_train_pred, dnn=c("Truth","Pred"))
##Performance Metric
conf_matrix <- confusionMatrix(factor(train$Revenue), y_train_pred)
# Extract metrics from the confusion matrix
precision <- conf_matrix$byClass["Pos Pred Value"]  # Precision
accuracy <- conf_matrix$overall["Accuracy"]         # Accuracy
recall <- conf_matrix$byClass["Recall"]
f1_score <- conf_matrix$byClass["F1"]               # F1 Score
cat("Precision:", precision, "\n")
cat("Accuracy:", accuracy, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Predicting the Test set results 
y_test_pred = predict(classifier, newdata = test[, 1:17])
table(test$Revenue, y_test_pred, dnn=c("Truth","Pred"))
##Performance Metric
conf_matrix <- confusionMatrix(factor(test$Revenue), y_test_pred)
# Extract metrics from the confusion matrix
precision <- conf_matrix$byClass["Pos Pred Value"]  # Precision
accuracy <- conf_matrix$overall["Accuracy"]         # Accuracy
recall <- conf_matrix$byClass["Recall"]
f1_score <- conf_matrix$byClass["F1"]               # F1 Score
cat("Precision:", precision, "\n")
cat("Accuracy:", accuracy, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")


#Probability of getting 1
test_prob_svm = predict(classifier, test, probability = TRUE)

#library(ROCR)
library(pROC)
# Create ROC curve
#num_Revenue = as.numeric(test$Revenue)
#test$Revenue = as.numeric(test$Revenue)
roc_curve <- roc(test$Revenue, as.numeric(test_prob_svm))
# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
#pred = prediction(test_prob_svm[,2], test$Revenue)
#perf = performance(pred, "tpr", "fpr")
#pred = prediction(list(test_prob_svm), test$Revenue)
#perf = performance(pred, "tpr", "fpr")
#plot(perf, colorize=TRUE)
# Add AUC value to the plot
text(0.8, 0.2, paste("AUC =", round(auc(roc_curve), 3)), col = "blue")
# Add a diagonal reference line for a random classifier
abline(a = 0, b = 1, lty = 2, col = "red")
# Add legend
legend("bottomright", legend = c("ROC Curve", "Random"), col = c("blue", "red"), lty = c(1, 2))
#AUC
slot(performance(pred, "auc"), "y.values")[[1]]

