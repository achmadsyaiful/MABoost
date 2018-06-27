
data<-read.delim("clipboard")
data1<-data

cor(data1)
head(train)
-----------------------------------------------------------------------------

set.seed(500)
acak <- sample(1:nrow(data1), 25000)
train <- data1[acak,]
test <- data1[-acak,]
disk.mdlp <-mdlp(train)
disk.mdlp$cutp

library(maboost)
library(caret)
library(discretization)
-----------------------------------------------------------------------------
  
default<-table(data1$X12)
pct <-round(default/sum(default)*100)
lbls<-c(0,1)
lbls <-paste(lbls, pct)
lbls <-paste(lbls,"%",sep="")
pie(default, labels = lbls)


par(mfrow=c(2, 3)) 
boxplot(data1$X1,main="X1")
boxplot(data1$X2,main="X2")
boxplot(data1$X3,main="X3")
boxplot(data1$X4,main="X4")
boxplot(data1$X5,main="X5")
boxplot(data1$X6,main="X6")
par(mfrow=c(2, 3)) 
boxplot(data1$X7,main="X7")
boxplot(data1$X8,main="X8")
boxplot(data1$X9,main="X9")
boxplot(data1$X10,main="X10")
boxplot(data1$X11,main="X11")
par(mfrow=c(1,1))

summary(data1)
sapply(data, sd, na.rm=TRUE) 
hist(data$X12)


-----------------------------------------------------------------------------
install.packages("randomForest")
install.packages("maboost")
install.packages("fastAdaboost")
install.packages("DMwR")
install.packages("ebmc")
install.packages("discretization")
install.packages("rpart.plot")
install.packages("cvTools")
install.packages("C50")
install.packages("rpart")
install.packages("lattice")
install.packages("ggplot2")
-----------------------------------------------------------------------------
#Maboost
fit_maboost <- maboost(X12~ X2+X5+X8+X9+X10+X11+X13+X14+X15+X16
                       ,data=train
                       ,iter = 100 
                       ,verbose = TRUE
                     )
prediksi.mb <- predict(fit_maboost, test, type = "class")  
confusionMatrix(factor(test$X12), factor(prediksi.mb), positive="1")

#Maboost down
  down_train <-downSample(x = train[, c(2:12,14:17)],y = as.factor(train$X12))
  model.maboost <- maboost(Class~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X13+X14+X15+X16
                           , data=down_train
                           , iter = 100 
                           ,verbose = TRUE)
  prediksi.mb <- predict(model.maboost, test)
  confusionMatrix(factor(test$X12), factor(prediksi.mb), positive="1")
  
#Maboost up
  up_train <-upSample(x = train[, c(2:12,14:17)],y = as.factor(train$X12))
  model.maboost <- maboost(Class~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X13+X14+X15+X16
                           ,data=up_train
                           ,iter = 100 
                           ,verbose = TRUE)
  prediksi.mb <- predict(model.maboost, test)
  confusionMatrix(factor(test$X12), factor(prediksi.mb), positive="1")

-----------------------------------------------------------------------------
#Diskretisasi
  disk <- function(var){
  var$x1_p<-ifelse(var$X1<=4.5,1,2)
  var$x2_p<-ifelse(var$X2<=9350.0  ,1
            ,ifelse(var$X2<=57627.5,2
            ,ifelse(var$X2<=36481.0,3
            ,ifelse(var$X2<=93546.5,4,5))))
  var$x3_p<-ifelse(var$X3<=0.5,1
            ,ifelse(var$X3<=7.5,2,3))
  var$x4_p<-ifelse(var$X4<=0.7748491,1
            ,ifelse(var$X4<=0.8304239,2,3))
  var$x5_p<-ifelse(var$X5<=0.01488258  ,1
            ,ifelse(var$X5<=0.87772980 ,2
            ,ifelse(var$X5<=0.96820958 ,3
            ,ifelse(var$X5<=0.99254028,4,5))))
  var$x6_p<-ifelse(var$X6<=109.5 ,1
            ,ifelse(var$X6<=2111.5,2
            ,ifelse(var$X6<=7732.0,3,4)))
  var$x7_p<-ifelse(var$X7<=0.9452338  ,1
            ,ifelse(var$X7<=0.9948161,2,3))
  var$x8_p<-ifelse(var$X8<=1.5  ,1
            ,ifelse(var$X8<=117.5 ,2
            ,ifelse(var$X8<=890.5 ,3
            ,ifelse(var$X8<=1660.5 ,4
            ,ifelse(var$X8<=3098.5 ,5,6)))))
  var$x9_p<-ifelse(var$X9<=3459.5 ,1
            ,ifelse(var$X9<=17240.0 ,2
            ,ifelse(var$X9<=28248.0  ,3
            ,ifelse(var$X9<=38914.0 ,4
            ,ifelse(var$X9<=66804.0,5,6)))))
  
  var$x10_p<-ifelse(var$X10<=0.15 ,1
            ,ifelse(var$X10<=3.50 ,2
            ,ifelse(var$X10<=5.50 ,3
            ,ifelse(var$X10<=7.85,4,5))))
  
  var$x11_p<-ifelse(var$X11<=0.15 ,1
             ,ifelse(var$X11<=1.85,2
             ,ifelse(var$X11<=2.5,3
             ,ifelse(var$X11<=3.5,4,5))))
  return(var)
} 
  
-----------------------------------------------------------------------------
-- #Modelling Diskretisasi
train_p <- disk(train)
test_p <-disk(test)

fit_maboost <- maboost(X12~ x1_p+x2_p+x3_p+x4_p+x5_p+x6_p+x7_p+x8_p+x9_p+x10_p+x11_p
                       , data=train_p
                       , iter = 100 ,verbose = TRUE)

prediksi.rf <- predict(fit_maboost, test_p)
confusionMatrix(factor(test_p$X12), prediksi.rf, positive="1")
