library(glmnet)
library(caret)
library(rpart)
library(klaR)
library(e1071)
library(caTools)
library(gbm)
library(ggplot2)
library(ggthemes)

student_mat <- read.csv(file.choose())
student_por <- read.csv(file.choose())
d3=merge(student_mat,student_por, all=TRUE)
print(nrow(d3)) 
d4 <- subset( d3, select = -c(G1, G2 ))

############################# Data visualization ################################

#Bar Chart of Mother's Job
ggplot(data = d4, aes(x = d4$Mjob)) +
  geom_histogram(bins = 12, stat = "count") + 
  #geom_density() + 
  ggtitle("Distribution of Mother's Job in student data") + 
  xlab("Mother's Job") +  
  ylab("Distribution of Mother's Job") 

#Mother's Job Bar chart segmented by Grades
ggplot(d4, aes(Mjob, fill=G3(title="Grades"))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position ='stack') + 
  ggtitle("Impact of Mother's Job on students' Grade") +
  labs(x="Mother's Job", y="Proportion") +
  theme_few()

#Impact of students' willingness to do Higher Education
ggplot(data = d4, aes(x = d4$higher)) +
  geom_histogram(bins = 12, stat = "count") +
  #geom_density() +
  ggtitle("Distribution of Higher Education in student data") +
  xlab("Higher Education") +
  ylab("Distribution of Higher Education")

#Distribution of Grade in student data
ggplot(data = d3, aes(x = d3$G3, col = "red")) +
  #geom_histogram(bins = 12, stat = "count") +
  geom_density() +
  ggtitle("Distribution of Grade in student data") +
  xlab("Grade") +
  ylab("Distribution of Grade")

#Distribution of previous failures in student data
ggplot(data = d4, aes(x = d4$failures, col = "red")) +
  #geom_histogram(bins = 12, stat = "count") +
  geom_density() +
  ggtitle("Distribution of previous failures in student data") +
  xlab("Failures") +
  ylab("Distribution of Failures")

## Correlation between G3 and failures
cor(d3$G3, d3$failures)
plot(x=d3$failures, y=d3$G3, xlab = "Failures", ylab = "Rank", col = "blue")

##Correlation between Age and G3
cor(d3$age, d3$G3)
plot(x=d3$age, y=d3$G3, xlab = "Age", ylab = "Rank", col = "red")

## Box plot of Rank vs Study Time
boxplot(d3$studytime ~ d3$G3, data = d3,  xlab = "Rank", ylab = "Study Time", main = "Box plot of Rank vs Study Time" ,col = "red")

##Building a kitchen sink linear regression model
set.seed (199)
trainindex=sample (1:nrow(d4), nrow(d4)/2)
train <- d4[trainindex, ]
test <- d4[-trainindex, ]
lm.model <- lm(G3 ~ ., data = train)
lm.y <- predict.lm(lm.model, test)
mean((lm.y - test$G3)^2)
test$G3[test$G3 >=0 & test$G3 <=5] <- "Low"
test$G3[test$G3 == 10 | test$G3 ==6 | test$G3 == 7 | test$G3 == 8 | test$G3 == 9] <- "BelowAverage"
test$G3[test$G3 >10 & test$G3 <=15] <- "Average"
test$G3[test$G3 >15 & test$G3 <=20] <- "High"
table(test$G3)
test$G3 <- as.factor(test$G3)
lm.y <- round(lm.y)
lm.y <- as.numeric(lm.y)
lm.y[lm.y >=0 & lm.y <=5] <- "Low"
lm.y[lm.y== 10 | lm.y ==6 | lm.y == 7 | lm.y == 8 | lm.y == 9] <- "BelowAverage"
lm.y[lm.y>10 & lm.y <=15] <- "Average"
lm.y[lm.y >15 & lm.y <=20] <- "High"
table(lm.y)
lm.y <- as.factor(lm.y)
regconfusion <- confusionMatrix(lm.y, test$G3)
accuracyReg <- regconfusion$overall[1]
accuracyReg

#function to retrieve the highest co eff
highestcoeff <- function(reg_model){
  sum_model <- summary(reg_model)
  sum_coeff <- sum_model$coefficients[,4]
  x <- max(sum_coeff)
  indexofhigh <- sum_coeff[sum_coeff==x]
  indexofhigh
}

##Correlation matrix with numbers
library(corrplot)
M <- cor(d4[,c(3,7,8,13,14,15,24:31)])
corrplot(M, method="number")


#linear regression with K-fold cross validation
d3=merge(student_mat,student_por, all=TRUE)
print(nrow(d3)) 
d4 <- subset( d3, select = -c(G1, G2 ))
set.seed(1234)
d4 <- d4[sample(nrow(d4)),]
folds <- cut(seq(1,nrow(d4)),breaks=3,labels=FALSE)
accuracyReg <- rep(0,3)
for(i in 1:3){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- d4[testIndexes, ]
  trainData <- d4[-testIndexes, ]
  lm.model <- lm(G3 ~ ., data = train)
  lm.y <- predict.lm(lm.model, test)
  test$G3[test$G3 >=0 & test$G3 <=5] <- "Low"
  test$G3[test$G3 == 10 | test$G3 ==6 | test$G3 == 7 | test$G3 == 8 | test$G3 == 9] <- "BelowAverage"
  test$G3[test$G3 >10 & test$G3 <=15] <- "Average"
  test$G3[test$G3 >15 & test$G3 <=20] <- "High"
  test$G3 <- as.factor(test$G3)
  lm.y <- round(lm.y)
  lm.y <- as.numeric(lm.y)
  lm.y[lm.y >=0 & lm.y <=5] <- "Low"
  lm.y[lm.y== 10 | lm.y ==6 | lm.y == 7 | lm.y == 8 | lm.y == 9] <- "BelowAverage"
  lm.y[lm.y>10 & lm.y <=15] <- "Average"
  lm.y[lm.y >15 & lm.y <=20] <- "High"
  lm.y <- as.factor(lm.y)
  regconfusion <- confusionMatrix(lm.y, test$G3)
  accuracyReg[i] <- regconfusion$overall[1]
}

#Lasso regression after variable selection
d4 <- d4[,-c(8,28)]
x=model.matrix (G3~., d4)[,-1]
y=d4$G3
set.seed (199)
train=sample (1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
lasso.mod = glmnet (x[train,], y[train], alpha =1)
cv.out = cv.glmnet (x[train,], y[train], alpha =1)
bestlam =cv.out$lambda.min
lasso.pred = predict (lasso.mod, s=bestlam, newx=x[test,])
mean(( lasso.pred - y.test)^2)
table(y.test)
y.test[y.test >=0 & y.test <=5] <- "Low"
y.test[y.test == 10 | y.test ==6 | y.test == 7 | y.test == 8 | y.test == 9] <- "BelowAverage"
y.test[y.test >10 & y.test <=15] <- "Average"
y.test[y.test >15 & y.test <=20] <- "High"
table(y.test)
y.test <- as.factor(y.test)
y.test = factor(y.test,levels(y.test)[c(1,2,4,3)])
predLassoValues <- round(lasso.pred[,1])
predLassoValues <- as.numeric(predLassoValues)
predLassoValues[predLassoValues >=0 & predLassoValues <=5] <- "Low"
predLassoValues[predLassoValues== 10 | predLassoValues ==6 | predLassoValues == 7 | predLassoValues == 8 | predLassoValues == 9] <- "BelowAverage"
predLassoValues[predLassoValues>10 & predLassoValues <=15] <- "Average"
predLassoValues[predLassoValues >15 & predLassoValues <=20] <- "High"
table(predLassoValues)
predLassoValues <- as.factor(predLassoValues)
levels(predLassoValues)[4] <- "High"
levels(predLassoValues)
table(predLassoValues)
table(y.test)
lassoregconfusion <- confusionMatrix(predLassoValues, y.test)
accuracyLasso <- lassoregconfusion$overall[1]
accuracyLasso
predict(lasso.mod, s=bestlam, type = "coefficients")[1:28,]

#Cateogorization of target variable before application of classification models
hist(d4$G3)
summary(d4$G3)
wed <- d4
d4$G3[d4$G3>=0 & d4$G3 <=5] <- "Low"
d4$G3[d4$G3==10] <- "BelowAverage"
d4$G3[d4$G3==9] <- "BelowAverage"
d4$G3[d4$G3==8] <- "BelowAverage"
d4$G3[d4$G3==7] <- "BelowAverage"
d4$G3[d4$G3==6] <- "BelowAverage"
d4$G3[d4$G3 > 10 & d4$G3 <=14] <- "Average"
d4$G3[d4$G3 >14 & d4$G3 <=20] <- "High"
d4$G3 <- as.factor(d4$G3)

#RP, NB & GB with 3-fold cross-validation
set.seed(1234)
d4 <- d4[sample(nrow(d4)),]
folds <- cut(seq(1,nrow(d4)),breaks=3,labels=FALSE)
accuracylist_nb <- rep(0,3)
accuracylist_rp <- rep(0,3)
accuracylist_gb <- rep(0,3)
for(i in 1:3){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- d4[testIndexes, ]
  trainData <- d4[-testIndexes, ]
  nbmodel <- naiveBayes(G3~., data = trainData)
  rpmodel <- rpart(G3~., data =trainData, cp=0)
  gbmodel <-  gbm(G3~., data=trainData, n.trees=200, interaction.depth=6, shrinkage=0.01)
  predictionNB <- predict(nbmodel, testData)
  predictionRP <- predict(rpmodel, testData, type = 'class')
  predictionGB <- predict(gbmodel, newdata=testData, n.trees=200, type="response")
  pred_class <- apply(predictionGB,1,which.max)
  pred_class[pred_class == 1] <- "Low"
  pred_class[pred_class == 2] <- "BelowAverage"
  pred_class[pred_class == 3] <- "Average"
  pred_class[pred_class == 4] <- "High"
  nboutput <- confusionMatrix(testData$G3, predictionNB)
  rpoutput <- confusionMatrix(testData$G3, predictionRP)
  gboutput <- confusionMatrix(testData$G3, pred_class)
  accuracylist_nb[i] <- paste0(nboutput$overall[1])
  accuracylist_rp[i] <- paste0(rpoutput$overall[1])
  accuracylist_gb[i] <- paste0(gboutput$overall[1])
}

#Visualizing the rpart tree
plot(rpmodel, compress = TRUE)
text(rpmodel, use.n = TRUE)

