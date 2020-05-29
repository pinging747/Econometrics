getwd()
install.packages("caret")
install.packages("data.table")
install.packages("cluster")
install.packages("factoextra")
install.packages("gridExtra")
install.packages("plyr")
install.packages("corrplot")
install.packages("unbalanced")
install.packages("DMwR")
install.packages("C50")
install.packages("gains")
install.packages("sqldf")
install.packages("dplyr")

library("caret")
library("data.table")
library("cluster")
library("factoextra")
library("gridExtra")
library("plyr")
library("corrplot")
library("unbalanced")
library("DMwR")
library("C50")
library("gains")
library("sqldf")
library(dplyr)

##setting wd

setwd("C:/MADAS/Dataset/ClustereChurn")


##importing data
training <-read.csv("training.txt",header=TRUE)
dep <-read.csv("deployment.txt",header=TRUE)


##removing NULL values in TotalCharges. Imputing not used because missing rows are relatively less
colSums(is.na(training))
colSums(is.na(dep))

training<-sqldf("select * from training
          where TotalCharges IS NOT NULL")
View(training)

dep<-sqldf("select * from dep
          where TotalCharges IS NOT NULL")
View(dep)


##structure check of data
str(training)

#coersion of data
training$customerID <- as.character(training$customerID)
training$SeniorCitizen <- as.factor(training$SeniorCitizen)
training$tenure <- as.numeric(training$tenure)


##fixing categorical variables levels
training$OnlineSecurity[training$OnlineSecurity == 'No internet service'] <- 'No'
training$OnlineSecurity<-droplevels(training$OnlineSecurity,3)
levels(training$OnlineSecurity)

training$OnlineBackup[training$OnlineBackup == 'No internet service'] <- 'No'
training$OnlineBackup<-droplevels(training$OnlineBackup,2)
levels(training$OnlineBackup)

training$DeviceProtection[training$DeviceProtection == 'No internet service'] <- 'No'
training$DeviceProtection<-droplevels(training$DeviceProtection,2)
levels(training$DeviceProtection)

training$TechSupport[training$TechSupport == 'No internet service'] <- 'No'
training$TechSupport<-droplevels(training$TechSupport,2)
levels(training$TechSupport)

training$StreamingTV[training$StreamingTV == 'No internet service'] <- 'No'
training$StreamingTV<-droplevels(training$StreamingTV,2)
levels(training$StreamingTV)

training$StreamingMovies[training$StreamingMovies == 'No internet service'] <- 'No'
training$StreamingMovies<-droplevels(training$StreamingMovies,2)
levels(training$StreamingMovies)

training$MultipleLines[training$MultipleLines == 'No phone service'] <- 'No'
training$MultipleLines<-droplevels(training$MultipleLines,2)
levels(training$MultipleLines)

#grouping tenure
min(training$tenure)
max(training$tenure)

?cut
training$tenure_group <- cut(training$tenure, breaks = c(0,12,24,48,60,max(training$tenure)),labels = c("0-12","13-24","25-48","49-60", "60+"), include.lowest = TRUE)
training$tenure_group <- as.factor(training$tenure_group)
View(training$tenure_group)

#new dataset train with customerid and tenure removed
train<-training
train$customerID<-NULL
train$tenure<-NULL
str(train)

##numerical variable correlation plot

numeric.var <- sapply(train, is.numeric)
corr.matrix <- cor(train[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

##since both numeric var are correlated, we remove one
train$MonthlyCharges<-NULL

##for coherence change seniorcitizen to No,Yes form
train$SeniorCitizen <- 
  as.factor(mapvalues(train$SeniorCitizen,
            from=c("0","1"),
            to=c("No", "Yes")))

##splitting data into train and test data
intrain<- createDataPartition(train$Churn,p=0.7,list=FALSE)
set.seed(2017)
tr<- train[intrain,]
test<- train[-intrain,]

##test to check if split is successful
dim(tr)
dim(test)


##trying Logistic Regression Model

LR <- glm(Churn~.,family=binomial(link="logit"),data=tr)
print(summary(LR))
View(LR)
anova(LR, test="Chisq")

#assessing the ability of model
test$Churn <- as.character(test$Churn)
test$Churn[test$Churn=="No"] <- "0"
test$Churn[test$Churn=="Yes"] <- "1"
fitted.results <- predict(LR,newdata=test,type='response')
fitted.results <- round(fitted.results, 0)
fitted.results <- as.character(fitted.results)
classification_error <- mean(fitted.results != test$Churn)
print(paste('Logistic Regression Accuracy',1-classification_error))

#confusion matrix
print("Confusion Matrix for Logistic Regression") 
table(test$Churn, fitted.results > 0.5)
#Odds ratio of Logistics Regression
exp(cbind(OR=coef(LR), confint(LR)))

##Decision Tree Model

tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, tr)
plot(tree, type='simple')

#confusion matrix
pred_tree <- predict(tree, test)
print("Confusion Matrix for Decision Tree"); 
table(Predicted = pred_tree, Actual = test$Churn)

#accuracy
p1 <- predict(tree, tr)
tab1 <- table(Predicted = p1, Actual = tr$Churn)
tab2 <- table(Predicted = pred_tree, Actual = test$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))


#random forest
install.packages("randomForest")
library(randomForest)
rfm <- randomForest(Churn ~., data = tr)
print(rfm)
rfm
#predition accurary test and confusion matrix
rfpred <- predict(rfm, test)
str(rfpred)
str(n)
n<-as.factor(test$Churn)
n<-as.factor(mapvalues(n,
                       from=c("0","1"),
                       to=c("No", "Yes")))

caret::confusionMatrix(rfpred, n)

plot(rfm)

#comparing the models, the best one based on accuracy is LR 0.81, DT 0.77, RFM 0.8
#so we choose logistic regression
#######FIRST PERFORM THE SAME STANDARDIZATION ON DEP
#coersion of data
dep$customerID <- as.character(dep$customerID)
dep$SeniorCitizen <- as.factor(dep$SeniorCitizen)
dep$tenure <- as.numeric(dep$tenure)


##fixing categorical variables levels
dep$OnlineSecurity[dep$OnlineSecurity == 'No internet service'] <- 'No'
dep$OnlineSecurity<-droplevels(dep$OnlineSecurity,3)
levels(dep$OnlineSecurity)

dep$OnlineBackup[dep$OnlineBackup == 'No internet service'] <- 'No'
dep$OnlineBackup<-droplevels(dep$OnlineBackup,2)
levels(dep$OnlineBackup)

dep$DeviceProtection[dep$DeviceProtection == 'No internet service'] <- 'No'
dep$DeviceProtection<-droplevels(dep$DeviceProtection,2)
levels(dep$DeviceProtection)

dep$TechSupport[dep$TechSupport == 'No internet service'] <- 'No'
dep$TechSupport<-droplevels(dep$TechSupport,2)
levels(dep$TechSupport)

dep$StreamingTV[dep$StreamingTV == 'No internet service'] <- 'No'
dep$StreamingTV<-droplevels(dep$StreamingTV,2)
levels(dep$StreamingTV)

dep$StreamingMovies[dep$StreamingMovies == 'No internet service'] <- 'No'
dep$StreamingMovies<-droplevels(dep$StreamingMovies,2)
levels(dep$StreamingMovies)

dep$MultipleLines[dep$MultipleLines == 'No phone service'] <- 'No'
dep$MultipleLines<-droplevels(dep$MultipleLines,2)
levels(dep$MultipleLines)

#grouping tenure
min(dep$tenure)
max(dep$tenure)

?cut
dep$tenure_group <- cut(dep$tenure, breaks = c(0,12,24,48,60,max(dep$tenure)),labels = c("0-12","13-24","25-48","49-60", "60+"), include.lowest = TRUE)
dep$tenure_group <- as.factor(dep$tenure_group)
View(dep$tenure_group)

#new dataset train with customerid and tenure removed
dep1<-dep
dep$customerID<-NULL
dep$tenure<-NULL
str(dep1)

dep1$MonthlyCharges<-NULL

##for coherence change seniorcitizen to No,Yes form
dep1$SeniorCitizen <- 
  as.factor(mapvalues(dep1$SeniorCitizen,
                      from=c("0","1"),
                      to=c("No", "Yes")))


##Applying Logistic Regression Model on deployment data

LR <- glm(Churn~.,family=binomial(link="logit"),data=tr)

churnpred<-predict(LR,dep1)
dep2<-cbind(dep1,churnpred)
View(dep2)
min(dep2$churnpred)
max(dep2$churnpred)
print(summary(LR))
View(LR)
anova(LR, test="Chisq")

#assessing the ability of model
prchurn <- predict(LR,newdata=dep1,type='response')
prchurn <- round(prchurn, 0)
View(prchurn)
dep2<-dep1
dep2$churn<-prchurn
View(dep2)

#probability distribution 
prob_churn<-(predict(LR,newdata=dep1,type='response'))*100
View(prob_churn)

##combine churn probability with deployment dataset

deployment<-read.csv("deployment.txt",header=TRUE)
View(deployment)
deployment1<-sqldf("select * from deployment
                    where TotalCharges IS NOT NULL")

?cbind

final<-cbind(deployment1,prob_churn)
View(final)

#churn probability customer text file

sql<- sqldf("select customerID, prob_churn from final
             order by prob_churn DESC")

allcust<-write.table(sql, "all.txt", sep="\t")



#find the top 100 customers for churn

customers<-sqldf("select * from final
                    Order By prob_churn DESC
                    Limit 100")
View(customers)

cust100<-write.table(customers$customerID, "top100.txt", sep="\t")

##############################changing churn score threshold
##############################ANSWER TO QUESTION 9
##trying Logistic Regression Model

LR <- glm(Churn~.,family=binomial(link="logit"),data=tr)
print(summary(LR))
View(LR)
anova(LR, test="Chisq")

#assessing the ability of model
test$Churn <- as.character(test$Churn)
test$Churn[test$Churn=="No"] <- "0"
test$Churn[test$Churn=="Yes"] <- "1"
fitted.results <- predict(LR,newdata=test,type='response')
View(fitted.results)

cutoffs <- seq(0.1,0.9,0.1)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  prediction <- ifelse(fitted.results >= cutoffs[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(test$Churn ==prediction))/length(prediction)*100)
}

plot(cutoffs, accuracy, pch =19,type='b',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")


##Thus from the above plot, we know that 50% is the optimum cutoff for highest accuracy
