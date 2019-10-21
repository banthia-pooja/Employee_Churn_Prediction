#Data Exploration and cleaning
#Including all the necessary library functions required in the project library(tidyverse)
library(gains) library(leaps) library(caret) library(dplyr) library(forecast) library(randomForest)

#Reading the csv datafile
Emp.df <- read.csv("HR_comma_sep.csv", strip.white = TRUE, blank.lines.skip = TRUE, header
                   = TRUE)

##Check to see if null values exist in any of the columns apply(Emp.df, 2, function(x) any(is.na(x)))

#Exploring the data str(Emp.df) levels(Emp.df$Departments) #Categorical data: table(Emp.df$left) count(Emp.df, Work_accident)
count(Emp.df, promotion_last_5years) #Data Visualizations
#converting left variable to factor variable hrm$left<-ifelse(hrm$left==1,'True','False') hrm$left<-factor(hrm$left,levels=c("True","False"))

#Histogram
p1<-ggplot(aes(x=satisfaction_level),data=hrm) + geom_histogram(color="black",fill="blue",bins = 30) +
  labs(title="Satisfaction level Histogram",x='Satisfaction Level of Employees', y="Frequency") p1

#Satisfaction level histogram facetted by salary classes p2 = p1 + facet_wrap(~salary)
p2

#Boxplot for Satisfaction level vs left facetted by Salary Ranges


ggplot(aes(x = left,y=satisfaction_level),data= hrm) + geom_boxplot() +
  ylab('Satisfaction Level') + xlab("Employee left") + facet_wrap(~salary) +
  labs(title = "Satisfaction level vs Churn facetted by Salary Ranges")

#faceted by whether an employee left or not ggplot(aes(x=number_project),data = hrm) + geom_bar(color='black',fill='blue') + xlab("Number of Projects") + ylab("Frequency") +
labs(title="Barplot of Number of projects faceted by Churn")+ facet_wrap(~left)

#Average monthly hours worked faceted by Churn
ggplot(aes(y = average_montly_hours, x = hrm$left),data=hrm)+ geom_boxplot() +
  xlab("Churn") +
  ylab("Average Monthly hours worked") +
  
  #Variable Time spend at Company faceted by Churn ggplot(aes(x = left, y = time_spend_company),data = hrm)+ xlab("Churn") +
  ylab("Variable Time spend at company in years")+
  labs(title = "Barplot of Variable Time spend at Company faceted by Churn")+ geom_boxplot()+
  labs(title="Average monthly hours worked faceted by Churn")

# Department and their count faceted by Salary ranges ggplot(aes(x =hrm$Departments),data = hrm ) + geom_bar(aes(fill=salary)) +
xlab('Department') + ylab('Counts') +
  labs(title = "Department and their count faceted by Salary ranges")

#Frequency of work accidents across Departments ggplot(aes(x = hrm$Departments),data = hrm) + geom_bar(aes(fill=factor(hrm$Work_accident))) + coord_flip() +
labs(x = "Department",y ="Frequency", fill="Work Accidents" , title = "Frequency of work accidents across Departments ")

##Number of projects


table(hrm$number_project,hrm$left) ##Employees who have worked on seven projects proj <- filter(hrm,number_project==7) summary(proj)

## Salary data for employees who have worked on 7 projects ggplot(aes(x = Departments),data = proj) + geom_bar(aes(fill=factor(salary))) +
coord_flip() +
  labs(x = "Departments",y ="Frequency", fill="Salary" )

#faceted by If a employee left or not ggplot(aes(x=number_project),data = hrm) + geom_bar(color='black',fill='#547398') + xlab("Number of Projects") + ylab("Frequency") +
labs(title="Barplot of Number of projects faceted by Left")+ facet_wrap(~left)

## Number of projects vs promotion facetted by left ggplot(aes(x = number_project),data = hrm) + geom_bar(aes(fill=factor(promotion_last_5years))) +
labs(x = "No. of projects",y ="Frequency", fill="Promotion" )+ facet_wrap(~left)

#ProjectCount VS Evaluation
#Looks like employees who did not leave the company had an average evaluation of around 70% even with different
#projectCounts.
#Employees that had two projects and a horrible evaluation left. Employees with more than 3 projects and super high evaluations
##left
p<-ggplot(hrm, aes(x = factor(number_project), y = last_evaluation, fill = factor(left))) + geom_boxplot() + scale_fill_manual(values = c("yellow", "orange"))
print(p)
##ProjectCount vs avaerage monthly hours
p<-ggplot(hrm, aes(x = factor(number_project), y = average_montly_hours, fill = factor(left))) + geom_boxplot() + scale_fill_manual(values = c("yellow", "orange"))
#print(p)[BOXPLOT]
#Looks like the average employees who stayed worked about 200hours/month. Those that had a turnover worked about 250hours/month
#and 150hours/month

#Correlation plot of variables:


#All int attributes have to be converted into numerics to feature in correlation matrix Emp.df$number_project<-as.numeric(Emp.df$number_project) Emp.df$average_montly_hours<-as.numeric(Emp.df$average_montly_hours) Emp.df$time_spend_company<-as.numeric(Emp.df$time_spend_company) Emp.df$Work_accident<-as.numeric(Emp.df$Work_accident)
Emp.df$left<-as.numeric(Emp.df$left)
Emp.df$promotion_last_5years<-as.numeric(Emp.df$promotion_last_5years) str(Emp.df)

c<-data.frame(Emp.df) correlation<-c[,-c(9:10)] m<-cor(correlation) library(corrplot) corrplot(m)

#Calculation of employee turn_over rate using Emp.left column #Turn_over rate = No. of employees who left / total no. of employees #where 1 indicates inactive (left) and 0 indicates active

turnover_rate = mean(Emp.df$left) turnover_rate
#Approximately 24 percent of employees are inactive (have left the firm) and 76 percent are active(currently employed at the firm)

#Turnover_rate with respect to each Department: Emp.df %>%
count(Departments, left)

df_Departments <- Emp.df %>% group_by(Departments) %>% summarize(turnover_Departments = mean(left))

#Plotting Department wise percentage turnover.
Department_tunover <- df_Departments$turnover_Departments*100
ggplot(df_Departments) + geom_bar(aes(x = Departments, y = Department_tunover), fill
                                  ="orange", width = 0.5, stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#We observe that management and RandD departments have the lowest turnover rates #HR department has the highest turnover rate followed by the Accounting department

#Linear regression set.seed(111)
#create data partition using predictor variable - left


train.index<- createDataPartition(Emp.df$left, p = 0.8, list =FALSE) train.df <- Emp.df[train.index,]
valid.df <- Emp.df[-train.index,]

### Run regression
lm.reg <- lm(left ~ ., data = train.df)

options(scipen = 999) sm <- summary(lm.reg)

### Generate predictions
lm.pred <- predict(lm.reg, valid.df) some.residuals <- valid.df$left - lm.pred plot(some.residuals, type = "p", pch = 16,
                                                                                    col = "blue1",
                                                                                    ylab = "Sample Residuals", ylim = c(0, 1), bty = "n", xlim = c(0, 100)
)

df <- data.frame("Predicted" = lm.pred, "Actual" = valid.df$left, "Residual" = some.residuals)
### Accuracy measures accuracy(lm.pred, valid.df$left) round(exp(coef(lm.reg)), 2)

lm.pred <- predict(lm.reg, valid.df[, -7], type = "response") t(t(head(lm.pred, 10)))

str(lm.pred)
confusion_matrix<- table(valid.df$left, lm.pred > 0.5) confusion_matrix

accuracy <- (sum(diag(confusion_matrix)) / sum(confusion_matrix)) accuracy

gain <- gains(valid.df$left, lm.pred, groups = 10)
#The accuracy of the Linear model is 0.7689, almost equal to 77 percent.

#Linear regression - Lift,Decile, ROC #Lift Chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$left))~c(0,gain$cume.obs),
     xlab = "No. of employees", ylab = "Cumulative of employees who left", col = "red", main = "Lift chart - Linear Regression", type = "l")


lines(c(0,sum(valid.df$left))~c(0, dim(valid.df)[1]), lty = 5)

# Decile-wise chart
heights <- gain$mean.resp/mean(valid.df$left)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,4), col = "purple", xlab = "Percentile", ylab = "Mean Response",
                     main = "Decile-wise lift chart")
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

#ROC curve library(pROC)
r <- roc(valid.df$left, lm.pred)
plot.roc(r, main = "Linear regression ROC") ### compute auc
auc(r)

#Area under the curve: 0.8232 #Logistic regression
#Splitting data into test and training data set.seed(111)
#create data partition using predictor variable - left
train.index<- createDataPartition(Emp.df$left, p = 0.8, list =FALSE) train.df <- Emp.df[train.index,]
valid.df <- Emp.df[-train.index,]
#when the dependent is categorical, we can use logistic regression
##to predict the probability of left - to see if the employee will stay or leave the company

log.reg <- glm(left ~ ., family ="binomial",data = train.df ) summary(log.reg)
round(exp(coef(log.reg)), 2)

log.reg.pred <- predict(log.reg, valid.df[, -7], type = "response") t(t(head(log.reg.pred, 10)))

str(log.reg.pred)
confusion_matrix<- table(valid.df$left, log.reg.pred > 0.5) confusion_matrix

accuracy <- (sum(diag(confusion_matrix)) / sum(confusion_matrix)) accuracy
gain <- gains(valid.df$left, log.reg.pred, groups = 10)
#We observe that the confusion matrix gives an accuracy of 77.9 ~ 78%

#Lift Chart


plot(c(0,gain$cume.pct.of.total*sum(valid.df$left))~c(0,gain$cume.obs),
     xlab = "No. of employees", ylab = "Cumulative of employees who left", col = "red", main = "Lift chart - Logistic Regression", type = "l")
lines(c(0,sum(valid.df$left))~c(0, dim(valid.df)[1]), lty = 5)

# Decile-wise chart
heights <- gain$mean.resp/mean(valid.df$left)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,4), col = "blue", xlab = "Percentile", ylab = "Mean Response",
                     main = "Decile-wise lift chart")
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

#ROC curve for Logistic regression ##ROC curve
library(pROC)
r <- roc(valid.df$left, log.reg.pred) plot.roc(r)
### compute auc auc(r)

#Linear Discriminant Analysis library(MASS)
library(caret) library(ggplot2) library(testthat) library(gains) library(lift)

hrm<-read.csv("HR_comma_sep.csv") View(hrm) as.numeric(hrm$Departments) as.numeric(hrm$salary)

#hrm$time_spend_company <- (hrm$time_spend_company*8760) set.seed(11)
training.index <- createDataPartition(hrm$left, p = 0.8, list = FALSE) hrm.train <- hrm[training.index, ]
hrm.valid <- hrm[-training.index, ]

### Linear Discriminant Analysis - use lda() from MASS package lda1 <- lda(left ~., data = train.df)
# output lda1
# predict


pred1 <- predict(lda1, newdata=hrm.valid, type ="response") lda.pred<- pred1$x
## class: predicted class
## posterior: posterior probabilities of belonging to different classes ## x: linear discriminant values

# check model accuracy
table(pred1$class, hrm.valid$left) # pred v actual mean(pred1$class == hrm.valid$left) # percent accurate gain <- gains(hrm.valid$left, pred1$x)
### cumulative lift chart options(scipen=999)
### Compute gains relative to employees left lefts <- hrm.valid$left[!is.na(hrm.valid$left)] sum(lefts)
plot(c(0,gain$cume.pct.of.total*sum(lefts))~c(0,gain$cume.obs),
     xlab="# of Employees", ylab="Cumulative of Employees who left", main="Lift Chart", col = "purple", type="l")

### baseline
lines(c(0,sum(lefts))~c(0,nrow(hrm.valid)), col="brown2", lty=1)

###decile chart
barplot(gain$mean.resp/mean(lefts), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart", col = "blue")
##ROC curve library(pROC)
r <- roc(hrm.valid$left, pred1$posterior[,2]) plot.roc(r)
### compute auc auc(r)

#Decision Tree

library(caret) library(gains) library(lift) library(rpart) library(rpart.plot)


hrm <- read.csv("HR_comma_sep.csv")

## Splitting Dataset into training and testing set.seed(111)
training.index <- createDataPartition(hrm$left, p = 0.8, list = FALSE) hrm.train <- hrm[training.index, ]
hrm.test <- hrm[-training.index, ] ## Generate classification tree
tree <- rpart(left ~ ., data = train.df, method = "class")
prp(tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10) predict1 <- predict(tree, hrm.test, type="class")

## Fully-Grown Tree
deeper.ct <- rpart(left ~ ., data = hrm.train, method = "class", cp = 0, minsplit = 1)

length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"]) # count number of leaves prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10,
box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))


## Complexity Parameters
## xval: # of folds to use cross-validation procedure
## CP: sets the smallest value for the complexity paraeter cv.ct <- rpart(left ~ ., data = hrm.train, method = "class",
cp = 0.00001, minsplit = 5, xval = 5) printcp(cv.ct)

## Pruned Classificatin Tree using CP with lowest error pruned.ct <- prune(cv.ct,
cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"]) length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"]) prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)

## Best-Pruned Tree set.seed(1)
cv.ct <- rpart(left ~ ., data = hrm.train, method = "class", cp = 0.00001, minsplit = 1, xval = 5)

printcp(cv.ct)
# Print out the cp table of cross-validation errors. pruned.ct <- prune(cv.ct, cp = 0.0154639)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))



predict1 <- as.numeric(predict1) gain <- gains(hrm.test$left, predict1) head(predict1)

## cumulative lift chart options(scipen=999)
## Compute gains relative to employees left lefts <- hrm.test$left[!is.na(hrm.test$left)] sum(lefts)
plot(c(0,gain$cume.pct.of.total*sum(lefts))~c(0,gain$cume.obs),
     xlab="# of Employees", ylab="Cumulative of Employees who left", main="Lift Chart", col = "purple", type="l")

## baseline
lines(c(0,sum(lefts))~c(0,nrow(hrm.test)), col="brown2", lty=1) ##ROC curve
library(pROC)
r <- roc(hrm.test$left, predict1) plot.roc(r)
### compute auc auc(r)

#Random Forest context1 <- Emp.df library(randomForest)
train.index <- createDataPartition(context1$left, p = 0.8, list = FALSE) train.df <- context1[train.index,]
valid.df <- context1[-train.index,] train.df$left <- as.character(train.df$left) train.df$left <- as.factor(train.df$left)
rf.reg <- randomForest(left ~., data = train.df) print(rf.reg)
pred_rf <- predict(rf.reg, valid.df, type = "prob") head(pred_rf)
x <- pred_rf[,2] library(pROC)
# Integrating all models into one ROC plot} library(prediction)
library(ROCR)
# List of predictions
pred_list <- list(log.reg.pred, lm.pred, lda.pred, predict1, x)

# List of actual values (same for all)


m <- length(pred_list) m
actual_list <- rep(list(valid.df$left), m)

# Plot the ROC curves
pred <- prediction(pred_list, actual_list) rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves") legend(x = "bottomright",
                                                                    legend = c("Logistic Regression", "Linear regression", "Linear Discriminant Analysis", "Decision Tree", "Random Forest"),
                                                                    fill = 1:m)
