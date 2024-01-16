##Breanna Smith, Rian Brooks   
# Mine Case Study  
# Math 489
# 2019

msha <- read.csv(file = "MSHA_Mine_Data.csv", header = TRUE)

#install.packages("caret")
library(caret)
set.seed(1234)
library(rpart)
library(rpart.plot)
library(MASS)
#install.packages("rpart.plot")
install.packages("rsq")
library(rsq)
#install.packages("tidyverse")
library(tidyverse)

summary(msha)
names(msha)

#delete missing values
data.nomissing <- msha[!is.na(msha$MINE_STATUS),]
data.nomissing <- data.nomissing[!is.na(data.nomissing$US_STATE),]
data.nomissing <- data.nomissing[!is.na(data.nomissing$PRIMARY),]
nrow(msha) - nrow(data.nomissing) #27 rows have missing values, so we remove them

#remove the PRIMARY variable since it is not needed
table(msha$PRIMARY)
data.reduced <- data.nomissing
data.reduced$PRIMARY <- NULL

#there are a lot of states, so just ignore that variable
table(msha$US_STATE)
data.reduced$US_STATE <- NULL

#take out the mines that are no longer in use
not_in_use <- c("Closed by MSHA","Non-producing","Permanently abandoned","Temporarily closed")
data.reduced.less <- data.reduced
data.reduced.less <- data.reduced[!(data.reduced$MINE_STATUS %in% not_in_use),]

#rename the mine status for the coal mines since they use different names
table(data.reduced.less$MINE_STATUS, data.reduced.less$COMMODITY)
data.reduced.less$NEW_MINE_STATUS[data.reduced.less$MINE_STATUS == "Active"] <- "Open"
data.reduced.less$NEW_MINE_STATUS[data.reduced.less$MINE_STATUS == "Full-time permanent"] <- "Open"
data.reduced.less$NEW_MINE_STATUS[data.reduced.less$MINE_STATUS == "Intermittent"] <- "Intermittent"
data.reduced.less$NEW_MINE_STATUS <- as.factor(data.reduced.less$NEW_MINE_STATUS)
#get rid of the original mine status column
data.reduced.less$MINE_STATUS <- NULL


#make the variable that we want to predict, injuries per 2000 hours
data.reduced.less$INJ_RATE <- data.reduced.less$NUM_INJURIES/(data.reduced.less$EMP_HRS_TOTAL/2000)

#make a plot to look at the spread of the injury rate
ggplot(data.reduced.less, aes(x = INJ_RATE)) + geom_histogram()
summary(data.reduced.less$INJ_RATE)

#the max is significantly larger than the mean, so look at the max point
data.reduced.less[data.reduced.less$INJ_RATE == 2000,]

#this person only worked 1 hour so look at the spread of employee hours
ggplot(data.reduced.less, aes(x = EMP_HRS_TOTAL)) + geom_histogram()

#closer look at the employees with smaller number of hours
# since there are so many
ggplot(data.reduced.less[data.reduced.less$EMP_HRS_TOTAL <= 10000,], aes(x = EMP_HRS_TOTAL)) + geom_histogram()

#some people worked very small hours but are still included,
# lets take out these people with not many hours per year
data.reduced.lesser <- data.reduced.less[data.reduced.less$EMP_HRS_TOTAL >= 2000,]
summary(data.reduced.lesser$EMP_HRS_TOTAL)

#took out people with large injury rate, data looks better
summary(data.reduced.lesser$INJ_RATE)
attach(data.reduced.lesser)

#check for outliers in other variables
table(INJ_RATE, YEAR)
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$YEAR == 2013])
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$YEAR == 2014])
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$YEAR == 2015])
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$YEAR == 2016])
#the mean is not very different for each year, possibly can remove them??
data.reduced.lesser$YEAR <- NULL

table(COMMODITY)
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$COMMODITY == "Coal"])
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$COMMODITY == "Metal"])
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$COMMODITY == "Nonmetal"])
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$COMMODITY == "Sand & gravel"])
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$COMMODITY == "Stone"])
#the means for the injury rate for each commodity are close to each other

table(TYPE_OF_MINE)
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$TYPE_OF_MINE == "Mill"])
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$TYPE_OF_MINE == "Sand & gravel"])
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$TYPE_OF_MINE == "Surface"])
summary(data.reduced.lesser$INJ_RATE[data.reduced.lesser$TYPE_OF_MINE == "Underground"])
#the mean injury rate for underground is higher than the injury rate for 
# the other types of mines.
#This could be a good variable to include in our model

#create testing and training set
partition <- createDataPartition(data.reduced.lesser$TYPE_OF_MINE, list = FALSE, p = .80)
train <- data.reduced.lesser[partition, ]
test <- data.reduced.lesser[-partition, ]

#log likelihood funciton from the code provided
LLfunction <- function(targets, predicted_values){
  p_v_zero <- ifelse(predicted_values <= 0, 0, predicted_values)
  p_v_pos <- ifelse(predicted_values <= 0, 0.000001 ,predicted_values)
  return(sum(targets*log(p_v_pos)) - sum(p_v_zero))
}

#look at the summary of the injury rates for the test and training set 
# to see if they are approximately the same
summary(train$INJ_RATE)
summary(test$INJ_RATE)

#they have similar means
var(train$INJ_RATE)
var(test$INJ_RATE)
#also have similar variance so will continue to build models

attach(train)

set.seed(153) # because rpart uses cross-validation for estimating complexity parameter

#Tree 1
tree.reduced <- rpart(cbind(EMP_HRS_TOTAL/2000, NUM_INJURIES) ~ . - EMP_HRS_TOTAL - INJ_RATE,
                      data = train,
                      method = "poisson",
                      control = rpart.control(minbucket = 25, 
                                              cp = 0, 
                                              maxdepth = 10))
#graphs the size of trees against the CP values
plotcp(tree.reduced)
tree.reduced.pruned <- prune(tree.reduced, 
                             cp = tree.reduced$cptable[which.min(tree.reduced$cptable[, "xerror"]), "CP"])
rpart.plot(tree.reduced.pruned)
printcp(tree.reduced.pruned)
tree.reduced.pruned

#predict injuries for training set and look how it compares to real values
pruned.predict <- (train$EMP_HRS_TOTAL/2000)*predict(tree.reduced.pruned, newdata = train, type = "vector") # The prediction for the loglikelihood function should be the number of injuries, not the injury rate
LLfunction(train$NUM_INJURIES,pruned.predict)

#predict injuries for testing set and look how it compares to real values
pruned.predict.test <- (test$EMP_HRS_TOTAL/2000)*predict(tree.reduced.pruned, newdata = test, type = "vector") # The prediction for the loglikelihood function should be the number of injuries, not the injury rate
LLfunction(test$NUM_INJURIES,pruned.predict.test)
#low LL for testing, so create a new model

#R-squared code
y_bar <- sum(test$NUM_INJURIES) / length(test$NUM_INJURIES)
SSR <- sum((pruned.predict.test- y_bar)^2)
SSE <- sum((test$NUM_INJURIES - pruned.predict.test))
SSTO <- SSR + SSE
R_sqrd <- SSR/SSTO
print(R_sqrd)


#TREE 2
#build another tree, this time playing around with the cp parameter
tree.reduced2 <- rpart(cbind(EMP_HRS_TOTAL/2000, NUM_INJURIES) ~ . - EMP_HRS_TOTAL - INJ_RATE,
                      data = train,
                      method = "poisson",
                      control = rpart.control(minbucket = 25, 
                                              cp = .0009, 
                                              maxdepth = 10))
#high CP gives smaller tree
plotcp(tree.reduced2)
tree.reduced.pruned2 <- prune(tree.reduced2, 
                             cp = tree.reduced2$cptable[which.min(tree.reduced2$cptable[, "xerror"]), "CP"])
rpart.plot(tree.reduced.pruned2)
printcp(tree.reduced.pruned2)
tree.reduced.pruned2

pruned.predict2 <- (train$EMP_HRS_TOTAL/2000)*predict(tree.reduced.pruned2, newdata = train, type = "vector") # The prediction for the loglikelihood function should be the number of injuries, not the injury rate
LLfunction(train$NUM_INJURIES,pruned.predict2)
#better LL than above
pruned.predict.test2 <- (test$EMP_HRS_TOTAL/2000)*predict(tree.reduced.pruned2, newdata = test, type = "vector") # The prediction for the loglikelihood function should be the number of injuries, not the injury rate
LLfunction(test$NUM_INJURIES,pruned.predict.test2)
#worse LL than above

#R-squared code
y_bar <- sum(test$NUM_INJURIES) / length(test$NUM_INJURIES)
SSR <- sum((pruned.predict.test2- y_bar)^2)
SSE <- sum((test$NUM_INJURIES - pruned.predict.test2))
SSTO <- SSR + SSE
R_sqrd <- SSR/SSTO
print(R_sqrd)


#TREE 3
#build another tree, this time playing with the maxdepth of the tree
tree.reduced3 <- rpart(cbind(EMP_HRS_TOTAL/2000, NUM_INJURIES) ~ . - EMP_HRS_TOTAL - INJ_RATE,
                       data = train,
                       method = "poisson",
                       control = rpart.control(minbucket = 25, 
                                               cp = .0009, 
                                               maxdepth = 5))
#high CP gives smaller tree
plotcp(tree.reduced3)
tree.reduced.pruned3 <- prune(tree.reduced3, 
                              cp = tree.reduced3$cptable[which.min(tree.reduced3$cptable[, "xerror"]), "CP"])
rpart.plot(tree.reduced.pruned3)
printcp(tree.reduced.pruned3)
tree.reduced.pruned3

pruned.predict3 <- (train$EMP_HRS_TOTAL/2000)*predict(tree.reduced.pruned3, newdata = train, type = "vector") # The prediction for the loglikelihood function should be the number of injuries, not the injury rate
LLfunction(train$NUM_INJURIES,pruned.predict3)
#better LL than above
pruned.predict.test3 <- (test$EMP_HRS_TOTAL/2000)*predict(tree.reduced.pruned3, newdata = test, type = "vector") # The prediction for the loglikelihood function should be the number of injuries, not the injury rate
LLfunction(test$NUM_INJURIES,pruned.predict.test3)
#worse LL than above

#R-squared code
y_bar <- sum(test$NUM_INJURIES) / length(test$NUM_INJURIES)
SSR <- sum((pruned.predict.test3- y_bar)^2)
SSE <- sum((test$NUM_INJURIES - pruned.predict.test3))
SSTO <- SSR + SSE
R_sqrd <- SSR/SSTO
print(R_sqrd)

#there are a lot of variables, see if we can cut those down
summary(train$PCT_HRS_UNDERGROUND)
summary(train$PCT_HRS_SURFACE)
summary(train$PCT_HRS_AUGER)
summary(train$PCT_HRS_CULM_BANK)
summary(train$PCT_HRS_DREDGE)
summary(train$PCT_HRS_MILL_PREP)
summary(train$PCT_HRS_OFFICE)
summary(train$PCT_HRS_OTHER_SURFACE)
summary(train$PCT_HRS_SHOP_YARD)
summary(train$PCT_HRS_STRIP)

ggplot(data = data.reduced.lesser, aes(x=AVG_EMP_TOTAL, y=NUM_INJURIES)) + geom_point()
ggplot(data = data.reduced.lesser, aes(x=PCT_HRS_AUGER, y=NUM_INJURIES)) + geom_point()
ggplot(data = data.reduced.lesser, aes(x=PCT_HRS_CULM_BANK, y=NUM_INJURIES)) + geom_point()
ggplot(data = data.reduced.lesser, aes(x=PCT_HRS_DREDGE, y=NUM_INJURIES)) + geom_point()
ggplot(data = data.reduced.lesser, aes(x=PCT_HRS_MILL_PREP, y=NUM_INJURIES)) + geom_point()
#mill prep would be a good variable I think
ggplot(data = data.reduced.lesser, aes(x=PCT_HRS_OFFICE, y=NUM_INJURIES)) + geom_point()
#office is a good variable
ggplot(data = data.reduced.lesser, aes(x=PCT_HRS_OTHER_SURFACE, y=NUM_INJURIES)) + geom_point()
ggplot(data = data.reduced.lesser, aes(x=PCT_HRS_SHOP_YARD, y=NUM_INJURIES)) + geom_point()
ggplot(data = data.reduced.lesser, aes(x=PCT_HRS_STRIP, y=NUM_INJURIES)) + geom_point()
#strip good
ggplot(data = data.reduced.lesser, aes(x=PCT_HRS_SURFACE, y=NUM_INJURIES)) + geom_point()
#surface good
ggplot(data = data.reduced.lesser, aes(x=PCT_HRS_UNDERGROUND, y=NUM_INJURIES)) + geom_point()
#underground good

table(data.reduced.lesser$COMMODITY)
levels(train$COMMODITY)
train$COMMODITY <- relevel(factor(train$COMMODITY), ref = "Sand & gravel")
levels(train$COMMODITY)

table(data.reduced.lesser$TYPE_OF_MINE)
levels(train$TYPE_OF_MINE)
train$TYPE_OF_MINE <- relevel(factor(train$TYPE_OF_MINE), ref = "Surface")
levels(train$TYPE_OF_MINE)

data.reduced.final <- data.reduced.lesser
data.reduced.final$MINE_TYPE <- paste(data.reduced.lesser$TYPE_OF_MINE, data.reduced.lesser$COMMODITY)
data.reduced.final$MINE_TYPE <- as.factor(data.reduced.final$MINE_TYPE)
data.reduced.final$MINE_TYPE <- relevel(data.reduced.final$MINE_TYPE, ref="Sand & gravel Sand & gravel")

summary(data.reduced.final[data.reduced.final$MINE_TYPE == "Sand & gravel Sand & gravel",])

partition <- createDataPartition(data.reduced.final$MINE_TYPE, list = FALSE, p = .80)
train <- data.reduced.final[partition, ]
test <- data.reduced.final[-partition, ]

summary(train$INJ_RATE)
summary(test$INJ_RATE)
#they have similar means
var(train$INJ_RATE)
var(test$INJ_RATE)


trainEND <- data.reduced.final[partition, ]
testEND <- data.reduced.final[-partition, ]

#code to create the GLM from the code provided

#Template model
glm.reduced0 <- glm(NUM_INJURIES ~  . -EMP_HRS_TOTAL,
                   family = poisson(),
                   offset = log(EMP_HRS_TOTAL/2000),
                   data = train)
summary(glm.reduced0)

glm.reduced <- glm(NUM_INJURIES ~  . -EMP_HRS_TOTAL  - TYPE_OF_MINE - COMMODITY - INJ_RATE - PCT_HRS_STRIP,
                   family = poisson(),
                   offset = log(EMP_HRS_TOTAL/2000),
                   data = train)
summary(glm.reduced)

#removing auger, culm_bank because their coefficients are NA
#if you remove shop_yard all of the other coefficients are no longer significant, same with surface
#pull out dredge and mill bank and then they become significant again 

glm.predict <- predict(glm.reduced, newdata = train, type = "response")
LLfunction(train$NUM_INJURIES,glm.predict)
glm.predict <- predict(glm.reduced, newdata = test, type = "response")
LLfunction(test$NUM_INJURIES,glm.predict)
#this works well for the training data, but not for the testing data

y_bar <- sum(test$NUM_INJURIES) / length(test$NUM_INJURIES)
SSR <- sum((glm.predict- y_bar)^2)
SSE <- sum((test$NUM_INJURIES - glm.predict))
SSTO <- SSR + SSE
R_sqrd <- SSR/SSTO
print(R_sqrd)

#---------------------------------
#(Step)
glm.step <- stepAIC(glm.reduced)
summary(glm.step)
glm.predict <- predict(glm.step, newdata = test, type = "response")
summary(glm.predict)
LLfunction(test$NUM_INJURIES,glm.predict)
ggplot(test, aes(x=NUM_INJURIES, y=glm.predict)) + geom_point() + xlab('Number of Injuries') + ylab('Predicted Number of Injuries') + ggtitle('GLM 4 Predicted vs Actual')

#Model 1 
#best general model
glm.reduced1 <- glm(NUM_INJURIES ~   SEAM_HEIGHT + AVG_EMP_TOTAL + PCT_HRS_AUGER + PCT_HRS_UNDERGROUND + PCT_HRS_DREDGE + PCT_HRS_OTHER_SURFACE + PCT_HRS_SHOP_YARD + PCT_HRS_MILL_PREP + PCT_HRS_OFFICE + NEW_MINE_STATUS + MINE_TYPE,
                   family = poisson(),
                   offset = log(EMP_HRS_TOTAL/2000),
                   data = train)
summary(glm.reduced1)

glm.step1 <- stepAIC(glm.reduced1)
summary(glm.step1)
glm.predict <- predict(glm.reduced1, newdata = train, type = "response")
LLfunction(train$NUM_INJURIES,glm.predict)
glm.predict <- predict(glm.reduced1, newdata = test, type = "response")
LLfunction(test$NUM_INJURIES,glm.predict)
#this model has a higher AIC but the LL is better for test
predict.df <- data.frame(glm.predict)
mean((test$NUM_INJURIES - predict.df$glm.predict)^2)
ggplot(test, aes(x=NUM_INJURIES, y=glm.predict)) + geom_point() + xlab('Number of Injuries') + ylab('Predicted Number of Injuries') + ggtitle('GLM 4 Predicted vs Actual')

#R-squared code
y_bar <- sum(test$NUM_INJURIES) / length(test$NUM_INJURIES)
SSR <- sum((glm.predict- y_bar)^2)
SSE <- sum((test$NUM_INJURIES - glm.predict))
SSTO <- SSR + SSE
R_sqrd <- SSR/SSTO
print(R_sqrd)


#Model 2
#commodity is pretty much the same as type of mine, so take it out
glm.reduced2 <- glm(NUM_INJURIES ~   SEAM_HEIGHT + AVG_EMP_TOTAL + PCT_HRS_AUGER + PCT_HRS_UNDERGROUND + PCT_HRS_DREDGE + PCT_HRS_OTHER_SURFACE + PCT_HRS_SHOP_YARD + PCT_HRS_MILL_PREP + PCT_HRS_OFFICE + NEW_MINE_STATUS + MINE_TYPE + AVG_EMP_TOTAL:PCT_HRS_STRIP,
                    family = poisson(),
                    offset = log(EMP_HRS_TOTAL/2000),
                    data = train)
summary(glm.reduced2)
glm.step2 <- stepAIC(glm.reduced2)
summary(glm.step2)

#Model 3
glm.reduced3 <- glm(NUM_INJURIES ~   SEAM_HEIGHT + AVG_EMP_TOTAL + PCT_HRS_AUGER + PCT_HRS_UNDERGROUND + PCT_HRS_DREDGE + PCT_HRS_OTHER_SURFACE + PCT_HRS_SHOP_YARD + PCT_HRS_MILL_PREP + PCT_HRS_OFFICE + NEW_MINE_STATUS + MINE_TYPE + AVG_EMP_TOTAL:PCT_HRS_STRIP + AVG_EMP_TOTAL:PCT_HRS_OFFICE,
                    family = poisson(),
                    offset = log(EMP_HRS_TOTAL/2000),
                    data = train)
summary(glm.reduced3)
glm.step3 <- stepAIC(glm.reduced3)
summary(glm.step3)

glm.predict3 <- predict(glm.step3, newdata = train, type = "response")
LLfunction(train$NUM_INJURIES,glm.predict3)
glm.predict3 <- predict(glm.reduced3, newdata = test, type = "response")
LLfunction(test$NUM_INJURIES,glm.predict3)

y_bar <- sum(test$NUM_INJURIES) / length(test$NUM_INJURIES)
SSR <- sum((glm.predict- y_bar)^2)
SSE <- sum((test$NUM_INJURIES - glm.predict))
SSTO <- SSR + SSE
R_sqrd <- SSR/SSTO
print(R_sqrd)

#---------------------------------

#Model 4
glm.reduced4 <- glm(NUM_INJURIES ~   SEAM_HEIGHT + AVG_EMP_TOTAL + PCT_HRS_AUGER + PCT_HRS_UNDERGROUND
                    + PCT_HRS_DREDGE + PCT_HRS_OTHER_SURFACE + PCT_HRS_SHOP_YARD + PCT_HRS_MILL_PREP + 
                      PCT_HRS_OFFICE + NEW_MINE_STATUS + MINE_TYPE + AVG_EMP_TOTAL:PCT_HRS_STRIP + 
                      AVG_EMP_TOTAL:PCT_HRS_OFFICE +AVG_EMP_TOTAL:PCT_HRS_MILL_PREP,
                    family = poisson(),
                    offset = log(EMP_HRS_TOTAL/2000),
                    data = train)
summary(glm.reduced4)

glm.predict4 <- predict(glm.reduced4, newdata = train, type = "response")
LLfunction(train$NUM_INJURIES,glm.predict3)
glm.predict4 <- predict(glm.reduced4, newdata = test, type = "response")
LLfunction(test$NUM_INJURIES,glm.predict4)

y_bar <- sum(test$NUM_INJURIES) / length(test$NUM_INJURIES)
SSR <- sum((glm.predict4- y_bar)^2)
SSE <- sum((test$NUM_INJURIES - glm.predict4))
SSTO <- SSR + SSE
R_sqrd <- SSR/SSTO
print(R_sqrd)


glm.step4 <- stepAIC(glm.reduced4)
summary(glm.step4)
#down to 41747 AIC
glm.predict.step4 <- predict(glm.step4, newdata = train, type = "response")
LLfunction(train$NUM_INJURIES,glm.predict.step4)
glm.predict.step4 <- predict(glm.step4, newdata = test, type = "response")
LLfunction(test$NUM_INJURIES,glm.predict.step4)
ggplot(test, aes(x=NUM_INJURIES, y=glm.predict)) + geom_point() + xlab('Injuries') + ylab('Predicted Injuries') + ggtitle('Step GLM 4 Predicted vs Actual')

y_bar <- sum(test$NUM_INJURIES) / length(test$NUM_INJURIES)
SSR <- sum((glm.predict.step4- y_bar)^2)
SSE <- sum((test$NUM_INJURIES - glm.predict.step4))
SSTO <- SSR + SSE
R_sqrd <- SSR/SSTO
print(R_sqrd)


#--------------------------------------------
#Model 5
#below model is same as 4
glm.reduced5 <- glm(NUM_INJURIES ~   SEAM_HEIGHT + AVG_EMP_TOTAL + PCT_HRS_AUGER + PCT_HRS_UNDERGROUND 
                    + PCT_HRS_DREDGE + PCT_HRS_OTHER_SURFACE + PCT_HRS_SHOP_YARD + PCT_HRS_MILL_PREP + 
                      PCT_HRS_OFFICE + NEW_MINE_STATUS + MINE_TYPE + AVG_EMP_TOTAL:PCT_HRS_STRIP + 
                      AVG_EMP_TOTAL:PCT_HRS_OFFICE +AVG_EMP_TOTAL:PCT_HRS_MILL_PREP,
                    family = poisson(),
                    offset = log(EMP_HRS_TOTAL/2000),
                    data = train)
summary(glm.reduced5)
glm.step5 <- stepAIC(glm.reduced5)
summary(glm.step5)
glm.predict <- predict(glm.reduced1, newdata = train, type = "response")
LLfunction(train$NUM_INJURIES,glm.predict)
glm.predict <- predict(glm.reduced1, newdata = test, type = "response")
LLfunction(test$NUM_INJURIES,glm.predict)
#not better with AVG_EMP:PCT_AUGER
#not better with AVG_EMP:PCT_OTHER
#not better with AVG_EMP:PCT_SHOP

#R-squared code
y_bar <- sum(test$NUM_INJURIES) / length(test$NUM_INJURIES)
SSR <- sum((glm.predict- y_bar)^2)
SSE <- sum((test$NUM_INJURIES - glm.predict))
SSTO <- SSR + SSE
R_sqrd <- SSR/SSTO
print(R_sqrd)

