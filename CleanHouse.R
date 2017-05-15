#Clean data for housing data
#Let's predict some house prices!
library(tidyverse)
library(randomForest)
library(mice)
library(modelr)
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

Find.NA.Vars <- function(data) {
  missing.vars <- lapply(data, function(x) {
    sum(is.na(x)) / 2919
  })
  missing.vars <- as.data.frame(missing.vars) 
  missing.vars <- as.data.frame(t(missing.vars))
  return(missing.vars) 
}

missing.vars <- Find.NA.Vars(full)
View(missing.vars)

#Let's drop some variables with > 30% NA and Utilities b/c >995 AllPub
full <- full %>%
  select(-c(PoolQC, MiscFeature, Alley, Fence, FireplaceQu, Utilities))

missing.vars <- Find.NA.Vars(full)
View(missing.vars)

VarWNA <- rownames(missing.vars$V1[missing.vars$V1 > 0])

VarWNA <- missing.vars %>%
  mutate(vars = rownames(missing.vars)) %>%
  filter(V1 > 0 & vars != 'SalePrice') %>%
  arrange(desc(V1))

View(VarWNA)

#Find log of sale price to reduce effects of large spread in prices
train$LogSalePrice[1:1460] <- log10(train$SalePrice[1:1460])

train %>%
  filter(GarageCond < 300) %>%
ggplot() + 
  geom_point(mapping = aes(x = GarageCond, y = LogSalePrice), na.rm = TRUE) +
  geom_hline(yintercept = median(train$LogSalePrice), color = "red")

# Looks like there is a positive rleationship and with >16% missing let's try imputing with MICE

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full, method = 'rf')  

mice_output <- mice::complete(mice_mod)

# Plot GarageCond distributions
par(mfrow = c(1,2))
  hist(full$GarageCond, freq = F, main = 'GarageCond: Original Data', 
     col = 'darkgreen', ylim = c(0,0.04))
hist(mice_output$GarageCond, freq = F, main = 'GarageCond: MICE Output', 
     col = 'lightgreen', ylim = c(0,0.04))


par(mfrow = c(1,1))
  barplot(prop.table(table(full$GarageCond)))
  barplot(prop.table(table(mice_output$GarageCond)))



md.pattern(full)
  
# Replace LotFrontage variable from the mice model.
full$LotFrontage <- mice_output$LotFrontage



#Replace all the rest of the NA with MICE values
full[,VarWNA$vars] <- mice_output[, VarWNA$vars]
str(full)

#find chr variables
var.class <- lapply(full, class)
var.class <- as.data.frame(var.class)
var.class <- as.data.frame(t(var.class))
View(var.class)

var.class <- var.class %>%
  mutate(var = row.names(var.class)) %>%
  filter(V1 == 'character') %>%
  select(var) 

# Turn chr into factors
full[var.class$var] <- lapply(full[var.class$var], function(x) as.factor(x))

missing.vars <- Find.NA.Vars(full)
View(missing.vars)

barplot(prop.table(table(full$Electrical)))
barplot(prop.table(table(full$GarageQual)))
###
###  Come back and impute the exterior1st and 2nd and Garage Qual
### Brute force adding values for Elec
full$Electrical[1380] <- 'SBrkr'

full <- full %>%
  select(-c(Exterior1st, Exterior2nd, GarageQual))

missing.vars <- Find.NA.Vars(full)
View(missing.vars)

var.class <- lapply(full, class)
var.class <- as.data.frame(var.class)
var.class <- as.data.frame(t(var.class))

### Data is ready to be made into a model!!
# Split back into train and test
train <- full[1:1460, ]
test <- full[1461:2919, ]

model.rf1 <- randomForest(SalePrice ~ ., data = train)
#error plot
plot(model.rf1)

# Variable Importance Plot
varImpPlot(model.rf1,
           sort = T,
           main="Variable Importance",
           n.var=20)

results <- as.data.frame(cbind(train$SalePrice, model.rf1$predicted))
results <- results %>%
  mutate(V1 = log(V1),
         V2 = log(V2),
         error = V1 - V2)

View(results)

#Ok, time to predict!

prediction <- predict(model.rf1, test)
str(prediction)

submission <- as.data.frame(prediction)
submission <- as.data.frame(cbind(test$Id, prediction), col.names = c("Id", "SalePrice"))

submission <- submission %>%
  rename(Id = V1, SalePrice = prediction)

View(submission)
write.csv(submission, file = "submissionRF1-5-14-17.csv", row.names = FALSE)
