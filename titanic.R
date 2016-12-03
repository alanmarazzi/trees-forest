### Full code for the "Don't get lost in a forest" post on rDisorder.eu

# Install needed packages
trees_packages <- c(
    "FFTrees",
    "party",
    "randomForest",
    "intubate",
    "dplyr",
    "gbm"
)
install.packages(trees_packages)

# Get the data (folder "data" in this repo) and load it. 
# I always avoid to convert all strings to factors
titanic <- read.csv(
    "https://raw.githubusercontent.com/alanmarazzi/trees-forest/master/data/train.csv", 
    stringsAsFactors = FALSE, 
    na.strings = "")

# Take a look at the structure of the dataset
str(titanic)

# The first thing I like to do is to convert all columns names to lowercase
names(titanic) <- tolower(names(titanic))

# sex and embarked to factors
titanic$sex <- as.factor(titanic$sex)

# Now convert to factor
titanic$embarked <- as.factor(titanic$embarked)

# The age variable has some missing values
mean(is.na(titanic$age))

# Deal with NA in age variable
age_prediction <- lm(age ~ survived + pclass + fare, data = titanic)
summary(age_prediction)
titanic$age[is.na(titanic$age)] <- predict(age_prediction,
    newdata = titanic[is.na(titanic$age),])

# Check NAs in age
sum(is.na(titanic$age))

# Remove variables that clearly have nothing to do with our prediction setting
# and run a logistic regression as a benchmark
library(dplyr)
library(intubate)

logi <- titanic %>% 
    select(survived, pclass, sex, age, sibsp) %>% 
    ntbt_glm(survived ~ ., family = binomial)

summary(logi)

# Predict on training and test set
logi_pred <- predict(logi, type = "response")
survivors_logi <- rep(0, nrow(titanic))
survivors_logi[logi_pred > .5] <- 1

# This is going to be our benchmark
table(model = survivors_logi, real = titanic$survived)
(480 + 250)/nrow(titanic)

# Now on the test set for submission on Kaggle
test <- read.csv("https://raw.githubusercontent.com/alanmarazzi/trees-forest/master/data/test.csv",
    stringsAsFactors = FALSE,
    na.strings = "")

names(test) <- tolower(names(test))
test$sex <- as.factor(test$sex)

test_logi_pred <- predict(logi, test, type = "response")
surv_test_logi <- data.frame(PassengerId = test$passengerid, 
    Survived = rep(0, nrow(test)))

surv_test_logi$Survived[test_logi_pred > .5] <- 1
table(surv_test_logi$Survived)
write.csv(surv_test_logi, "results/logi.csv", row.names = FALSE)
# Result is 0.77512

# Temporary copy to avoid masking from FFTrees package
titanicc <- titanic

library(FFTrees)
titanic <- titanicc
rm(titanicc)

# Fast and Frugal Trees model building
fftitanic <- titanic %>% 
    select(age, pclass, sex, sibsp, fare, survived) %>% 
    ntbt(FFTrees, survived ~ .)

# Plotting of the best tree
plot(fftitanic, 
     main = "Titanic", decision.names = c("Not Survived", "Survived"))

# Build a simple classifier out of the best tree
ffpred <- ifelse(test$sex != "male", 1,
                 ifelse(test$pclass > 2, 0,
                        ifelse(test$fare < 26.96, 0,
                               ifelse(test$age >= 21.36, 0, 1))))


ffpred[is.na(ffpred)] <- 0
ffpred <- data.frame(PassengerId = test$passengerid, Survived = ffpred)
write.csv(ffpred, "results/fftree.csv", row.names = FALSE)
# Result is 0.76555

library(party)

partyTitanic <- titanic %>% 
    select(age, pclass, sex, sibsp, fare, survived) %>% 
    ntbt(ctree, as.factor(survived) ~ .)

plot(partyTitanic)

train_party <- Predict(partyTitanic)

table(tree = train_party, real = titanic$survived)

party_pred <- Predict(partyTitanic, newdata = test)

party_pred <- as.numeric(party_pred) - 1

party_pred <- data.frame(PassengerId = test$passengerid, 
                         Survived = party_pred)
write.csv(party_pred, "results/party.csv", row.names = FALSE)
# Result is 0.73684

# Bagging
library(randomForest)
set.seed(123)
titanic_bag <- titanic %>% 
    select(survived, age, pclass, sex, sibsp, fare, parch) %>% 
    ntbt_randomForest(as.factor(survived) ~ ., mtry = 6)

test$age[is.na(test$age)] <- median(test$age, na.rm = TRUE)
    
bag_pred <- predict(titanic_bag, test)

sum(is.na(bag_pred))
bag_pred[is.na(bag_pred)] <- 1
bag_pred <- data.frame(PassengerId = test$passengerid, 
                       Survived = bag_pred, 
                       row.names = 1:length(bag_pred))
write.csv(bag_pred, "results/bagging.csv", row.names = FALSE)
# Result is 0.66507

# RandomForest
set.seed(456)
titanic_rf <- titanic %>% 
    select(survived, age, pclass, sex, sibsp, fare, parch) %>% 
    ntbt_randomForest(as.factor(survived) ~ ., mtry = 3, n.trees = 5000)

rf_pred <- predict(titanic_rf, test)
rf_pred[is.na(rf_pred)] <- 1
rf_pred <- data.frame(PassengerId = test$passengerid, Survived = rf_pred, row.names = 1:length(rf_pred))
write.csv(rf_pred, "results/rf.csv", row.names = FALSE)
# Result is 0.74641

# RF with inferential trees
set.seed(415)
titanic_rf_party <- titanic %>% 
    select(survived, age, pclass, sex, sibsp, fare, parch) %>% 
    ntbt(cforest, as.factor(survived) ~ ., 
            controls = cforest_unbiased(ntree = 5000, mtry = 3))

rf_party_pred <- predict(titanic_rf_party, 
                         test, 
                         OOB = TRUE, 
                         type = "response")
rf_party_pred <- data.frame(PassengerId = test$passengerid, 
                            Survived = rf_party_pred)
write.csv(rf_party_pred, "results/rf_party.csv", row.names = FALSE)
# Result is 0.77033

# Boosting
library(gbm)
set.seed(999)
titanic_boost <- titanic %>% 
    select(survived, age, pclass, sex, sibsp, fare, parch) %>% 
    ntbt(gbm, survived ~ .,
         distribution = "bernoulli",
         n.trees = 5000,
         interaction.depth = 3)

boost_pred <- predict(titanic_boost, test, n.trees = 5000, type = "response")
test_boost <- rep(0, nrow(test))
test_boost[boost_pred >= .5] <- 1
test_boost <- data.frame(PassengerId = test$passengerid,
                         Survived = test_boost)
write.csv(test_boost, "results/test_boost.csv", row.names = FALSE)
# Result is 0.76077