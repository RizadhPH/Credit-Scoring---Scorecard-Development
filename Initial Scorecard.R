# Load some packages for data manipulation: 
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)

# Import data: 
data <- read.csv("....../creditdata.csv")

# NULL counts for each columns 
na_count <- sapply(data, function(x) sum(length(which(is.na(x)))))
na_count <- data.frame(na_count)
na_count

categorical <- function(x) {
  x %>% 
    table() %>% 
    as.data.frame() %>% 
    arrange(-Freq) ->> my_df
  
  n_obs <- sum(my_df$Freq)
  pop <- my_df$. %>% as.character()
  set.seed(29)
  x[is.na(x)] <- sample(pop, sum(is.na(x)), replace = TRUE, prob = my_df$Freq)
  return(x)
}

replace_by_mean <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}
data <- data %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(REASON = case_when(REASON == "" ~ NA_character_, TRUE ~ REASON), 
         JOB = case_when(JOB == "" ~ NA_character_, TRUE ~ JOB)) %>%
  mutate_if(is_character, as.factor) %>% 
  mutate_if(is.numeric, replace_by_mean) %>% 
  mutate_if(is.factor, categorical)

# Train - Test Split
sample_size <- floor(0.6 * nrow(data))
#setting the seed to make the partition reproducible
set.seed(123)
train_index <- sample(seq_len(nrow(data)), size = sample_size)
train <- data[train_index, ]
test <- data[-train_index, ]
dim(train)
dim(test)
#--------------------------Feature Selection-----------------------------------------------------
mod <- lm(BAD ~ ., data = train)
modBIC <- MASS::stepAIC(mod, k = log(nrow(train)))
summary(modBIC)
modAIC <- MASS::stepAIC(mod, k = 2)
summary(modAIC)
AICattr <- attr(terms(modAIC), "term.labels")
data_AIC <- train[,c("BAD",AICattr)]
model <- lm(BAD ~ ., data = data_AIC)
modBIC <- MASS::stepAIC(model, k = log(nrow(data_AIC)))
BICattr <- attr(terms(modBIC), "term.labels")
data_BIC <- data_AIC[,c("BAD",BICattr)]
head(data_BIC)

# 8 features selected for scorecard
#-----------------------Scorecard Development--------------------------------------------------------
library(scorecard)
bins_var <- woebin(data_BIC, y = "BAD", no_cores = 20, positive = "BAD|1")
data_train_woe <- woebin_ply(data_BIC, bins_var)
logistic <- glm(BAD ~ ., family = binomial, data = data_train_woe)
logistic %>% summary()
scorecards <- scorecard(bins_var, logistic, points0 = 600, odds0 = 1/19, pdo = 50)
