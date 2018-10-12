install.packages("questionr")
library(question)
library(tree)
library(tinytex)
library(leaps)
library(ElemStatLearn)
library(ggplot2)
library(questionr)
library(caret)
library(klaR)
library(ISLR)
library(ElemStatLearn)
library(kernlab)
library(randomForest)
#Read in the Datafile
exercise <- read.csv("pml-training.csv", header=TRUE)
fix(exercise)
head(exercise)
#Split the data into a testing and training dataset
inTrain <-createDataPartition(y=exercise$classe, p=0.75, list=FALSE)
training <- exercise[inTrain,]
testing <- exercise[-inTrain,]
dim(training)
dim(testing)
#The aformentioned leaves us with 14718 subsects for training
#and 4904 for testing.
#create a new table with the variables we have chosen to use
head(exercise)
exercise <- subset(exercise, select=c(classe,total_accel_belt, total_accel_forearm, total_accel_arm, total_accel_dumbbell))
head(exercise)
tail(exercise)
inTrain <-createDataPartition(y=exercise$classe, p=0.75, list=FALSE)
training <-exercise[inTrain,]
testing <- exercise[-inTrain,]
dim(training)
dim(testing)
#We have now created a new data set which we will be using going forward to build our prediciton model.
#We will use K Folds sampling to created 20 folds for our 20 predictions.
set.seed(32323)
folds <- createFolds(y=training$classe, k=20, list=TRUE, returnTrain=TRUE)
sapply(folds,length)
#The aforementioned represent the 20 folds we have created and their length.
#Now we will create 20 datasets through resampling.
set.seed(32323)
folds <- createResample(y=training$classe, times=20, list=TRUE)
sapply(folds, length)
#Creating a Feature Plot
featurePlot(x=training[,c("classe","total_accel_belt", "total_accel_forearm", "total_accel_arm", "total_accel_dumbbell")],
            y = training$classe, plot="pairs")
#Create a Qplot
qplot(classe,total_accel_belt,colour=classe, data=training)
plot
#Create covariates/dummy variables to use in case you need
table(training$classe)
dummies <-dummyVars(total_accel_belt ~ classe, data=training)
head(predict(dummies, newdata=training))
head(training)


lml <- lm(classe ~., data=training[1:1000,])
lml
summary(lml)

predicted <- predict(lml, testexercise[1:1000, ], type = "response")
predicted
testexercise$classe = as.numeric(testexercise$classe)
actual <- testexercise[1:1000, "classe" ]
sqrt(mean(predicted - actual)^2)

#Predict with regression example
inTrain <- createDataPartition(y=exercise$classe, p=0.5, list=FALSE)
trainexercise <- exercise[inTrain,}
testexercise <- exercise[-inTrain,]
head(trainexercise)/
plot(trainexercise$classe, trainexercise$total_accel_arm, pch=19, col="blue", xlab="Classe", ylab="Arm Acceleration")
#create a linear model between two numeric variables
lml <- lm(classe ~., data=trainexercise)
lml
summary(lml)
predicted <- predict(lml, data=testexercise, typ = "response")
predicted

actual <- exercise[testexercise, "class"]
sqrt(mean(predicted - actual)^2)
actual

#We want to predict classe
fix(exercise)
names(exercise)
regfit.full=regsubsets(classe~.,exercise)
summary(regfit.full)
#based on the results, the best model with have two variables
regfit.full=regsubsets(classe~.,data=exercise, nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
#Shows the increase in R square, only the first two variables seem important
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylabl="RSS")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq")
#Graphic display of R squared
which.max(reg.summary$adjr2)
#Location of maximum point of the vector
coef(regfit.full, 4)
#one addtional graph for perspective
qplot(total_accel_belt, total_accel_forearm, colour=classe, data=exercise)
#linear model (training random forest)
modFit <- train(classe ~ total_accel_belt, total_accel_forearm, total_accel_arm, total_accel_dumbell, data=exercise)
print(modFit)
#Let's predict with a tree
tree.exercise=tree(classe~., data=exercise)
summary(tree.exercise)
#error rate was 62%
plot(tree.exercise)
text(tree.exercise,pretty=0)
predict(tree.exercise,newdata=exercise)
#bagging (bootstrap aggregating) example
#bagging is a special case of random forest with m=p
set.seed(1)
bag.exercise=randomForest(classe~.,data=exercise)
bag.exercise
importance(bag.exercise)
#Gini = total variance acrsos the K classes
#Random Forest
modFit <- train(classe ~.,data=exercise, method="rf", prox=TRUE)
#Unsupervised Prediction
fix(exercise)
head(exercise)
inTrain <- createDataPartition(y=exercise$classe, p=0.7, list=FALSE)
training <- exercise[inTrain,]
testing <- exercise[-inTrain,]
results <- kmeans(exercise.features, 4)
table(exercise$classe, results$cluster)
plot(exercise[c("total_accel_belt","total_accel_forearm")],col=results$cluster)
table(results$cluster)
modFit <- train(classe~.,data=subset(training, selsect=c(classe)), method= "rpart")
table(predict(modFit, training), training$classe)
testClusterPred <- predict(modFit, testing)
table(testClusterPred, testing$classe)


names(trainexercise)
