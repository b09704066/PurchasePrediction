data = read.csv('/Users/nina/Downloads/online_shoppers_intention.csv')

#Classification Tree
library(rpart.plot)

# convert categorical data to factor
data$Weekend<- factor(data$Weekend)
data$TrafficType <- factor(data$TrafficType)
data$OperatingSystems <- factor(data$OperatingSystems)
data$Region <- factor(data$Region)
data$Browser <- factor(data$Browser)
data$Month <- factor(data$Month)
data$VisitorType <- factor(data$VisitorType)
data$Revenue <- factor(data$Revenue) #############

#oversample
library(ROSE)
#Oversample the minority class using ROSE
#over_data <- ovun.sample(Revenue ~., data = data, 
                                #method = "over", N = 10 * sum(data$Revenue == TRUE))$data

over_data <- ovun.sample(Revenue ~., data = data, method = "over")$data

#Train-test split
index <- sample(nrow(over_data),nrow(over_data)*0.80)
train = over_data[index,]
test = over_data[-index,]

#Classification Tree
data_rpart <- rpart(formula = Revenue ~ . , data = train, method ="class",
                    parms = list(loss=matrix(c(0,5,1,0), nrow = 2)))

#Pruning
largetree1 <- rpart(formula = Revenue ~ ., data = train, method ="class",
                    cp = 0.001, parms = list(loss=matrix(c(0,5,1,0), nrow = 2)))
prp(largetree1)
printcp(largetree1)
#in-sample performance for large tree
train.pred.largetree1<- predict(largetree1, train, type="class")
table(train$Revenue, train.pred.largetree1, dnn=c("Truth","Pred"))

plotcp(largetree1) #find out appropriate cp (complexity parameter)

largetree2 <- rpart(formula = Revenue ~ ., data = train, method ="class",
                    cp = 0.001, parms = list(loss=matrix(c(0,5,1,0), nrow = 2)))
prp(largetree2)
#in-sample performance for tree after pruning
train.pred.largetree2<- predict(largetree2, train, type="class")
table(train$Revenue, train.pred.largetree2, dnn=c("Truth","Pred"))

##print tree
data_rpart <- largetree2
prp(data_rpart, extra = 1)

#prediction (in-sample)
##asymmetric
train.pred.tree1<- predict(data_rpart, train, type="class")
table(train$Revenue, train.pred.tree1, dnn=c("Truth","Pred"))

#prediction (out-of-sample)
test.pred.tree1<- predict(data_rpart, test, type="class")
table(test$Revenue, test.pred.tree1, dnn=c("Truth","Pred"))

#Cost function
cost <- function(r, phat){
  weight1 <- 5
  weight0 <- 1
  pcut <- weight0/(weight1+weight0)
  #phat_class1 <- phat[, 2]
  c1 <- (r==1)&(phat<pcut) #logical vector - true if actual 1 but predict 0
  c0 <-(r==0)&(phat>pcut) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}

##calculate cost
#data$Revenue <- as.logical(data$Revenue)
#data$Revenue <- as.numeric(data$Revenue)
#data$Revenue <- data$Revenue * 1

cost(train$Revenue, predict(data_rpart, train, type="prob"))
cost(test$Revenue, predict(data_rpart, test, type="prob"))

#Probability of getting 1
test_prob_rpart = predict(data_rpart, test, type="prob")

library(ROCR)
pred = prediction(test_prob_rpart[,2], test$Revenue)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#AUC
slot(performance(pred, "auc"), "y.values")[[1]]

#cut-off
test_pred_rpart = as.numeric(test_prob_rpart[,2] > 1/(5+1))
table(test$Revenue, test_pred_rpart, dnn=c("Truth","Predicted"))
