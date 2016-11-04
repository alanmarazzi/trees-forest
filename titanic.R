### Full code for the "Don't get lost in a forest" post on rDisorder.eu

# Install needed packages
trees_packages <- c(
    "FFTrees",
    "evtree",
    "party",
    "randomForest"
)
install.packages(trees_packages)

# Get the data (folder "data" in this repo) and load it. 
# I always avoid to convert all strings to factors
titanic <- read.csv("data/train.csv", stringsAsFactors = FALSE)

# Take a look at the structure of the dataset
str(titanic)

# The first thing I like to do is to convert all columns names to lowercase
names(titanic) <- tolower(names(titanic))

# sex and embarked to factors
titanic$sex <- as.factor(titanic$sex)

# By checking embarked we see that there are some "" empty characters
# We want them as NA and not as a level
# First check all unique values of embarked
unique(titanic$embarked)

# Assign NAs to "" empty character
titanic$embarked[titanic$embarked == ""] <- NA

# Now convert to factor
titanic$embarked <- as.factor(titanic$embarked)

# Remove variables that clearly have nothing to do with our prediction setting
library(dplyr)
titanic_mod <- titanic %>% 
    select(survived, pclass, sex, age, sibsp, parch, fare)

# Our benchmark will be a logistic regression
logi <- glm(survived ~ . - fare - parch, data = titanic_mod, family = binomial)
summary(logi)

# Predict on training and test set
logi_pred <- predict(logi, type = "response")
survivors_logi <- rep(0, nrow(titanic))
survivors_logi[logi_pred > .5] <- 1

# This is going to be our benchmark
table(model = survivors_logi, real = titanic$survived)

