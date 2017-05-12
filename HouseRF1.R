#Let's predict some house prices!
library(tidyverse)
library(randomForest)
library(mice)

#setwd
setwd("~/Rprojects/house-prices")

#load in data
train <- read.csv("./data/train.csv", stringsAsFactors = TRUE)
test <- read.csv("./data/test.csv", stringsAsFactors = TRUE)
View(test)

#train IDs are 1-1460 (1460)
#test IDs are 1461-2919 (1459)

full <- bind_rows(train, test)
View(full)

missing.vars <- lapply(full, function(x) {
  sum(is.na(x)) / 2919
})

missing.vars <- as.data.frame(t(missing.vars)) 
View(missing.vars)

#Let's drop some variables with > 30% NA
full <- full %>%
  select(-c(PoolQC, MiscFeature, Alley, Fence, FireplaceQu))

missing.vars <- lapply(full, function(x) {
  sum(is.na(x)) / 2919
})

missing.vars <- as.data.frame(missing.vars)
missing.vars <- as.data.frame(t(missing.vars))
View(missing.vars)

train$LogSalePrice[1:1460] <- log10(train$SalePrice)

BxPlot <- function(z, data = train) {
  ggplot(data = data, mapping = aes(x = factor(z), y = LogSalePrice)) + 
    geom_boxplot() +
    geom_hline(yintercept = median(train$LogSalePrice), color = "red")
}
BxPlot(train$Street)
BxPlot(train$Utilities)
BxPlot(z = train$OverallCond)

sum(train$Utilities == "AllPub") / length(train$Utilities)

ggplot(train, mapping = aes(x = factor(MoSold), y = LogSalePrice)) + 
  geom_boxplot() +
  geom_hline(yintercept = median(train$LogSalePrice), color = "red")

ggplot(train, mapping = aes(x = factor(OverallCond), y = LogSalePrice)) + 
  geom_boxplot() +
  geom_hline(yintercept = median(train$LogSalePrice), color = "red")

ggplot(train, mapping = aes(x = factor(MSZoning), y = LogSalePrice)) + 
  geom_boxplot() +
  geom_hline(yintercept = median(train$LogSalePrice), color = "red")

ggplot(train, mapping = aes(x = factor(MSZoning), y = LogSalePrice)) + 
  geom_boxplot() +
  geom_hline(yintercept = median(train$LogSalePrice), color = "red")

train %>%
  filter(LotFrontage < 300) %>%
ggplot(mapping = aes(x = LotFrontage, y = LogSalePrice)) +
  geom_point()
  
