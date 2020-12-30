install.packages("AER")
library(AER)
data("Affairs",package = "AER")
library(moments)

Affairs <- read.csv(file.choose())
#("C:/RAVI/Data science/Assignments/Mo  dule 9 Logistic regression/LR Assignment dataset1/Affairs.csv/Affairs.csv")

#write.csv(Affairs, file = "affairss111.csv", col.names = F, row.names = F)
#getwd()


View(Affairs)
summary(Affairs)
attach(Affairs)

skewness(Affairs$affairs)
skewness(Affairs$age)
plot(Affairs)
colnames(Affairs)
class(Affairs)


#transform affairs column into a dichotomous factor called ynaffair with the following code.

Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair, 
                           levels=c(0,1),
                           labels=c("No","Yes"))



table(Affairs$ynaffair)


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
#(GLM)generalised linear model

model <- glm(ynaffair ~ gender+age+yearsmarried+children+religiousness+education+occupation+rating, data = Affairs, family = "binomial")
summary(model)

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
prob <- predict(model,Affairs,type="response")
prob
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion <- table(prob > 0.5, Affairs$ynaffair)
confusion

# Model Accuracy 
Accuracy <- sum(diag(confusion)/sum(confusion))
Accuracy 

Specificity =435/(435+16)=0.964 #TN/(TN+FP) OR True_Negetive_Rate
Sensitivity <-25/(25+125)=0.16  #TP/(TP+FN) OR True_Positive_Rate
False_Positive_Rate <- 1-Specificity=0.03


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL


pred_values <- ifelse(prob > 0.5, 1, 0)
yes_no <- ifelse(prob > 0.5,"yes","no")

# Creating new column to store the above values
Affairs[ , "prob"] <- prob
Affairs[ , "pred_values"] <- pred_values
Affairs[ , "yes_no"] <- yes_no

View(Affairs[ ,c(1,11:12)])

table(Affairs$ynaffair,Affairs$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity


# ROC(Receiver perativing characteristic) Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic

install.packages("ROCR")
library(ROCR)

rocrpred <- prediction(prob,Affairs$ynaffair)
rocrpred
rocrperf <- performance(rocrpred,'tpr','fpr')
rocrperf


plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)

rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)

# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

##################
modelfit <- glm(ynaffair ~ gender+age+yearsmarried+children+religiousness+rating, data = Affairs, family = "binomial")
summary(modelfit) #this is the best model
