

#More variables need to be dropped... Let's read about it: https://www.analyticsvidhya.com/blog/2015/07/dimension-reduction-methods/

#Find log of sale price to reduce effects of large spread in prices
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
  
#Let's just throw this in a RF and see what happens
#split the data back into train and test

train <- full[1:1460,]
test <- full[1461:2919,]

no.na.train <- train %>%
  na.omit()

rf.model1 <- randomForest(SalePrice ~ ., data = no.na.train)

