dataset123 <- read.csv("E:\\Programming\\R Codes\\Mini_Project\\Classification\\Blood_Donation.csv")

column_names <- c('Recency_Months', 'Frequency', 'Monetary', 'Overall_Months', 'Donation_Rate')
colnames(dataset) <- column_names


#R (Recency - months since last donation), 
#F (Frequency - total number of donation), 
#M (Monetary - total blood donated in c.c.), 
#T (Time - months since first donation), and 
#a binary variable representing whether he/she donated blood in March 2007 (1 stand for donating blood; 0 stands for not donating blood). 1

#-------------------------------------------------------------------------------------------#
#Principle Component Analysis

cor(dataset[,1:4])
library(corrplot)
corrplot(cor(dataset[,1:4]))
x <- dataset[,1:4]
x <- as.matrix(dataset[,1:4])

scaled_x <- scale(x)

dataset_ori <- dataset 
PCA_dataset <- princomp(~scaled_x, scores = TRUE, cor=TRUE)
#View Proportion of Variables
summary(PCA_dataset)

##View the loadings for each variable across the Factors
loadings(PCA_dataset)
Factors <- loadings(PCA_dataset)
Factors
print(Factors, digits = 2, cutoff = 0.5, sort=TRUE)


##View the Factor Scores
Y <- PCA_dataset$scores

##Scree plot
plot(PCA_dataset, type="line")

View(dataset_ori[,5])
View(PCA_dataset)


#Converted_Dataset <- data.frame(PCA_dataset)
Converted_Dataset <- data.frame(Y)


#Merging a dataframe
Final_dataset <- cbind(Converted_Dataset,dataset_ori[,5])

#-------------------------------------------------------------------------------------------#

#Checking Null Values in Dataset
sapply(dataset, function(x) sum(is.na(x)))

#-------------------------------------------------------------------------------------------#

column_names <- c('Frequency_Monetary', 'Recency_Months', 'Overall_Months', 'FandM','Donation_Rate')
colnames(Final_dataset) <- column_names

str(test_set) 
test_set$Donation_Rate <- as.factor(test_set$Donation_Rate)
str(training_set$Donation_Rate)
training_set$Donation_Rate <- as.factor(training_set$Donation_Rate)

#-----------------------------------------------------------------------------------------#

#Model Creation

#Logistic Regression

#Spliting Train and Test data

library(caTools)
set.seed(123)
split = sample.split(Final_dataset$Donation_Rate, SplitRatio = 0.75)
training_set = subset(Final_dataset, split == TRUE)
test_set = subset(Final_dataset, split == FALSE)

#Creating Full Model

Model_Donation <- glm(Donation_Rate ~ Frequency_Monetary + Recency_Months + Overall_Months ,family = "binomial", data = training_set)
summary(Model_Donation)
backwards = step(Model_Donation)
Full_Model <- glm(Donation_Rate ~ .,  family = 'binomial', data = training_set)
summary(Full_Model)
backwards = step(Full_Model)

#Finding out the probability Value

#ChurnPrediction<- predict(Log_Model_7, newdata = ChurnTest1, type="response") #0.8147
PredictBloodDonation<- predict(Model_Donation, newdata = test_set[-5], type="response")

#ChurnPrediction <- round(ChurnPrediction)

#Cross Validation - Confustion Matrix

table(test_set$Donation_Rate,PredictBloodDonation>0.5)

library(caret)
install.packages("e1071")
PredictBloodDonation <- round(PredictBloodDonation)
PredictBloodDonation <- as.factor(PredictBloodDonation)
confusionMatrix(test_set$Donation_Rate,PredictBloodDonation)
str(test_set$Donation_Rate)
str(PredictBloodDonation>0.5)


str(test_set) 
test_set$Donation_Rate <- as.factor(test_set$Donation_Rate)
str(training_set$Donation_Rate)
training_set$Donation_Rate <- as.factor(training_set$Donation_Rate)
str(PredictBloodDonation)

#-----------------------------------------------------------------------------------------#


#Plotting ROC Curve

install.packages("ROCR")
library(ROCR)
ROC_DonationPredict<-prediction(PredictBloodDonation, test_set$Donation_Rate)
ROC_DonationPerformance<-performance(ROC_DonationPredict, measure="tpr", x.measure = "fpr")
plot(ROC_DonationPerformance, col = "Red")

str(PredictBloodDonation)
str(test_set$Donation_Rate)
#-------------------------------------------------------------------------------------------#


#Performance Curve Value

auc<-performance(ROC_DonationPredict, measure="auc")
auc<-auc@y.values[[1]]
auc #0.7473

#-------------------------------------------------------------------------------------------#
#C50 Codes - Decision Tree

View(Final_dataset)
str(Final_dataset)
Final_dataset$Donation_Rate <- as.factor(Final_dataset$Donation_Rate)

library(caTools)
set.seed(123)
split = sample.split(Final_dataset$Donation_Rate, SplitRatio = 0.75)
Tree_training_set = subset(Final_dataset, split == TRUE)
Tree_test_set = subset(Final_dataset, split == FALSE)


install.packages("C50")
library(C50)

#Buliding Decision Tree Model
classifier_rules <- C5.0(Donation_Rate~., data = Tree_training_set, rules = TRUE)
classifier_plots <- C5.0(Donation_Rate~., data = Tree_training_set)
#classifier_Ex <- C5.0(Donation_Rate~Recency_Months + Overall_Months + Frequency_Monetary, data = Tree_training_set)
summary(classifier_rules)
plot(classifier_plots)
plot(classifier_Ex)

pred <- predict(classifier, newdata = Tree_test_set[-5])
table(Tree_test_set[,5],pred)
pred <- as.factor(pred)
str(pred)
confusionMatrix(Tree_test_set[,5],pred)

#Accuracy = 0.7796
install.packages("CORElearn")
install.packages("RWeka")
install.packages("FSelector")
install.packages("rJava")
library(CORElearn)
library(RWeka)
library(FSelector)

IG.CORElearn <- information.gain(Donation_Rate ~ ., data=Tree_training_set)
IG.RWeka     <- InfoGainAttributeEval(Donation_Rate ~ ., data=Tree_training_set)


a <- gain.ratio(classifier, Tree_training_set)
#------------------------------------------------------------------------------------------#

#rPart Codes - Decision Tree

View(Final_dataset)
str(Final_dataset)
Final_dataset$Donation_Rate <- as.factor(Final_dataset$Donation_Rate)

library(caTools)
set.seed(123)
split = sample.split(Final_dataset$Donation_Rate, SplitRatio = 0.75)
Tree_training_set = subset(Final_dataset, split == TRUE)
Tree_test_set = subset(Final_dataset, split == FALSE)

install.packages("rpart")
library(rpart)
classifier_rpart <- rpart(Donation_Rate~., data = Tree_training_set)
classifier_rpart$variable.importance
plot(classifier_rpart)

pred <- predict(classifier_rpart, newdata = Tree_test_set[-5], type = "class")
table(Tree_test_set$Donation_Rate,pred)

confusionMatrix(Tree_test_set$Purchased,pred)
pred <- as.factor(pred)
str(pred)

pred <- as.factor(pred)
str(pred)
confusionMatrix(Tree_test_set[,5],pred)
#Accuracy = 0.7473

#-----------------------------------------------------------------------------------------#

#Random Forest

install.packages("party")
install.packages("randomForest")
# Load the party package. It will automatically load other required packages.
library(party)
library(randomForest)

# Create the forest.
output.forest <- randomForest(Donation_Rate~.,data = Tree_training_set,ntree=1000,mtry=3,importance=T)

varImpPlot(output.forest)

# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(fit,type = 2))


pred_random = predict(output.forest,newdata = Tree_test_set[-5],type = "class")
pred_random <- as.factor(pred_random)
str(pred_random)
confusionMatrix(Tree_test_set[,5],pred)
#Accuracy = 0.7473

#-----------------------------------------------------------------------------------------#

#Nave Bayes

View(Final_dataset)
str(Final_dataset)

library(caTools)
set.seed(123)
split = sample.split(Final_dataset$Donation_Rate, SplitRatio = 0.75)
Bayes_training_set = subset(Final_dataset, split == TRUE)
Bayes_test_set = subset(Final_dataset, split == FALSE)
str(Bayes_training_set)
str(Bayes_test_set)


library(e1071)

Bayes_classifier = naiveBayes(x = Bayes_training_set[-5],
                        y = Bayes_training_set$Donation_Rate)
summary(Bayes_classifier)
# Predicting the Test set results
y_pred = predict(Bayes_classifier, newdata = Bayes_test_set[-5])

# Making the Confusion Matrix
cm = table(Bayes_test_set[, 5], y_pred)

confusionMatrix(Bayes_test_set[,5],y_pred)

#accuracy = 0.7581

#------------------------------------------------------------------------------------------#
