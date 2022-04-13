library(psych)
library(e1071)
library(dplyr)
library(ggplot2)
library(class)
library(caret)
library(corrplot)
# install.packages("gridExtra")
library(gridExtra)
#data preparation
wine <- read.csv("/Users/Hello/Downloads/winequality-white.csv", sep = ";", head = TRUE)
head(wine)
dim(wine)
attach(wine)
wine[,1]<-sapply(wine[,1],as.numeric)
# Reclassify quality into 2 categories
wine %>% ggplot(aes(x = quality))+ geom_histogram()
wine <- wine %>% mutate(y = as.factor((ifelse(quality <= 5, 0, 1))))
wine %>% ggplot(aes(x = y))+ geom_bar(width = 0.2)
colnames(wine)
# Matrix Scatter plot
pairs.panels(wine[, 1:11],
             method = "pearson", # correlation method
             hist.col = "steelblue",
             pch = 21, bg = c("pink", "light green", "light blue")[(iris$Species)], density = TRUE, # show 
             density plots
             ellipses = FALSE # show correlation ellipses
)
# Boxplots
p1 <- wine %>% ggplot(aes(group = y, y = fixed.acidity)) + geom_boxplot()
p2 <- wine %>% ggplot(aes(group = y, y = volatile.acidity)) + geom_boxplot()
p3 <- wine %>% ggplot(aes(group = y, y = citric.acid)) + geom_boxplot()
p4 <- wine %>% ggplot(aes(group = y, y = residual.sugar)) + geom_boxplot()
p5 <- wine %>% ggplot(aes(group = y, y = chlorides)) + geom_boxplot()
p6 <- wine %>% ggplot(aes(group = y, y = free.sulfur.dioxide)) + geom_boxplot()
p7 <- wine %>% ggplot(aes(group = y, y = total.sulfur.dioxide)) + geom_boxplot()
p8 <- wine %>% ggplot(aes(group = y, y = density)) + geom_boxplot() + ylim(0.98, 1.01)
p9 <- wine %>% ggplot(aes(group = y, y = pH)) + geom_boxplot()
p10 <- wine %>% ggplot(aes(group = y, y = sulphates)) + geom_boxplot()
p11 <- wine %>% ggplot(aes(group = y, y = alcohol)) + geom_boxplot()
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, nrow = 3) # Put boxplots on one page
t.test(filter(wine, y == 1)$alcohol, filter(wine, y == 0)$alcohol, paired = FALSE)
# Split the data
set.seed(100)
training.idx <- sample(1: nrow(wine), size=nrow(wine)*0.8)
train.data <-wine[training.idx, ]
test.data <- wine[-training.idx, ]
dim(train.data)
dim(test.data)
# Estimate Density
## KNN Regression
set.seed(101)
knn_model1 <- train(
  density~
    fixed.acidity+ 
    volatile.acidity+ 
    citric.acid+
    residual.sugar+ 
    chlorides+ 
    free.sulfur.dioxide+ 
    total.sulfur.dioxide+
    pH+
    sulphates, 
  data = train.data, method = "knn",
  trControl = trainControl("cv", number = 100),
  preProcess = c("center","scale"),
  tuneLength = 10)
plot(knn_model1)
knn_model1$bestTune
predictions <- predict(knn_model1, test.data)
head(predictions)
RMSE(predictions, test.data$density)
par(mfrow=c(1,1))
plot(test.data$density, predictions,main="Prediction performance of kNN regression")
abline(0,1, col="red")
## Linear Regression
lmodel <- lm(density~fixed.acidity+ volatile.acidity+ citric.acid+
               residual.sugar+ chlorides+ free.sulfur.dioxide+ total.sulfur.dioxide+
               pH+ sulphates, data = train.data)
summary(lmodel)
predictions <-predict(lmodel, test.data)
plot(test.data$density, predictions, main="Prediction performance of linear regression")
abline(0,1, col="red")
RMSE(predictions, test.data$density)
par(mfrow=c(2,2)) 
plot(lmodel)
# Improved: Eliminate outliers and attempt on second-order terms
par(mfrow=c(1,1)) 
corrplot(cor(train.data[, 1:11] %>% 
               mutate(residual.sugar_sq = residual.sugar^2, 
                      total.sulfur.dioxide_sq = total.sulfur.dioxide^2,
                      residual_sulfur = residual.sugar*total.sulfur.dioxide)), 
         type="upper", method="color",addCoef.col = "black",number.cex = 0.6)
# Eliminate Outliers
wine1 <- wine[-c(1527, 3266, 485, 3902, 4481),]
set.seed(105)
training.idx1 <- sample(1: nrow(wine1), nrow(wine1)*0.8) 
train.data1 <- wine1[training.idx1, ]
test.data1 <- wine1[-training.idx1, ]
lmodel2 <- lm(density~fixed.acidity+ volatile.acidity+ citric.acid+
                residual.sugar+ chlorides+ free.sulfur.dioxide+ total.sulfur.dioxide+
                pH+ sulphates, data = train.data1)
summary(lmodel2)
predictions2 <-predict(lmodel2, test.data1)
RMSE(predictions2, test.data1$density) 
plot(test.data1$density, predictions2, main="Prediction performance of linear regression")
abline(0, 1, col = "red")
par(mfrow = c(2, 2))
plot(lmodel2)
# Estimate Alcohol
# KNN Regression
set.seed(101)
knn_model2 <- train(
  alcohol~
    fixed.acidity+ 
    volatile.acidity+ 
    citric.acid+
    residual.sugar+ 
    chlorides+ 
    free.sulfur.dioxide+ 
    total.sulfur.dioxide+
    pH+
    density+
    sulphates, 
  data = train.data, method = "knn",
  trControl = trainControl("cv", number = 100),
  preProcess = c("center","scale"),
  tuneLength = 10)
plot(knn_model2)
knn_model2$bestTune
predictions <- predict(knn_model2, test.data)
head(predictions)
RMSE(predictions, test.data$alcohol)
par(mfrow=c(1,1))
plot(test.data$alcohol, predictions,main="Prediction performance of kNN regression")
abline(0,1, col="red")
# Linear Regression
lmodel <- lm(alcohol~fixed.acidity+ volatile.acidity+ citric.acid+
               residual.sugar+ chlorides+ free.sulfur.dioxide+ total.sulfur.dioxide+
               density+ pH+ sulphates, data = train.data)
summary(lmodel)
predictions <-predict(lmodel, test.data)
plot(test.data$alcohol, predictions, main="Prediction performance of linear regression")
abline(0,1, col="red")
RMSE(predictions, test.data$alcohol)
par(mfrow=c(2,2)) 
plot(lmodel)
par(mfrow=c(1,1)) 
# corrplot(cor(train.data[, 1:11]), type="upper", method="color",addCoef.col = "black",number.cex = 0.6)
corrplot(cor(train.data[, 1:11] %>% 
               mutate(density_sq = density^2, 
                      total.sulfur.dioxide_sq = total.sulfur.dioxide^2,
                      residual.sugar_sq = residual.sugar^2,
                      residual_sulfur = residual.sugar*total.sulfur.dioxide,
                      density_residual = density*residual.sugar,
                      sulfur_density = density*total.sulfur.dioxide)), 
         type="upper", method="color",addCoef.col = "black",number.cex = 0.6)
wine2 <- wine[-c(1527,1664,3902,2782),]
set.seed(106)
training.idx2 <- sample(1: nrow(wine2), nrow(wine2)*0.8) 
train.data2 <- wine1[training.idx2, ]
test.data2 <- wine1[-training.idx2, ]
lmodel2 <- lm(alcohol~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+
                chlorides+total.sulfur.dioxide+I(density^2)+
                sulphates+I(total.sulfur.dioxide^2)+I(residual.sugar^2)+
                residual.sugar*total.sulfur.dioxide+density*residual.sugar+
                density*total.sulfur.dioxide, data = train.data2)
summary(lmodel2)
predictions2 <-predict(lmodel2, test.data2)
RMSE(predictions2, test.data2$alcohol)
par(mfrow = c(2, 2))
plot(lmodel2)
par(mfrow = c(1, 1))
plot(test.data2$alcohol, predictions2, main="Prediction performance of linear regression")
abline(0,1, col="red")
# Logistic Regression (All variables)
mlogit<-glm(y~.,data=train.data %>% select(-quality),family="binomial")
summary(mlogit)
pred.p<-predict(mlogit,newdata=test.data,type="response")
y_pred_num<-ifelse(pred.p>0.5,1,0)
y_pred<-factor(y_pred_num,levels=c(0,1))
tab<-table(y_pred,test.data$y)
tab
mean(y_pred==test.data$y)
# logistic regression (selected variables)
mlogitn<-glm(y~ volatile.acidity + residual.sugar + free.sulfur.dioxide + density + pH + 
               sulphates + alcohol, data = train.data, family="binomial")
summary(mlogitn)
pred.p<-predict(mlogitn,newdata=test.data,type="response")
y_pred_numn<-ifelse(pred.p>0.5,1,0)
y_pred<-factor(y_pred_numn,levels=c(0,1))
mean(y_pred==test.data$y)
# KNN
nor<-function(x){(x-min(x))/(max(x)-min(x))}
nor_wine <- nor(wine[, 1:11])
wine1 <- cbind(wine, nor_wine)
# dim(wine1): 4898 24
set.seed(110)
knn1<-knn(wine1[, 14:24][training.idx, ], wine1[, 14:24][-training.idx, ], cl=wine1[training.idx, ]$y, k=2)
mean(knn1==test.data$y)
#find the best k
ac<-rep(0,30)
for(i in 1:30){
  set.seed(110)
  knn.i<-knn(wine1[, 14:24][training.idx, ], wine1[, 14:24][-training.idx, ], cl=wine1[training.idx, ]$y, k=i)
  ac[i]<-mean(knn.i==test.data$y)
  cat("k=",i,"accuracy=",ac[i],"\n")
}
par(mfrow = c(1, 1))
plot(ac,type="b",xlab="k",ylab="accuracy")
# SVM
set.seed(100)
m.svm1<-svm(y ~ ., data = train.data %>% dplyr::select(-quality), type = "C-classification", 
            kernel = "linear")
summary(m.svm1)
pred.svm1 <- predict(m.svm1, newdata=test.data %>% dplyr::select(-quality))
table(pred.svm1, test.data$y)
mean(pred.svm1 == test.data$y)
set.seed(2)
m.svm.tune1<-tune.svm(y ~ ., data=train.data %>% dplyr::select(-quality), type = "C-classification", 
                      kernel="radial", cost=10^(-1:2), gamma=c(.1,.5,1,2))
summary(m.svm.tune1)
plot(m.svm.tune1)
best.svm1 = m.svm.tune1$best.model
best.svm1
pred.svm.tune1 = predict(best.svm1, newdata=test.data %>% dplyr::select(-quality))
table(pred.svm.tune1, test.data$y)
mean(pred.svm.tune1 == test.data$y)