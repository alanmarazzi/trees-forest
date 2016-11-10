### Full code for the "Don't get lost in a forest" post on rDisorder.eu

# Install needed packages
trees_packages <- c(
    "FFTrees",
    "evtree",
    "party",
    "randomForest",
    "intubate",
    "dplyr"
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

party_pred <- Predict(partyTitanic, newdata = test)

party_pred <- as.numeric(party_pred) - 1

party_pred <- data.frame(PassengerId = test$passengerid, Survived = party_pred)
write.csv(party_pred, "results/party.csv", row.names = FALSE)
# Result is 0.73684