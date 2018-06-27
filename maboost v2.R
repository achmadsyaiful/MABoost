
library(cvTools)
library(maboost)
library(caret)
library(discretization)

data<-read.delim("clipboard")
data1<- data
acak <- sample(1:nrow(data), 25000)
train <- data1[acak,]
test <-data1[-acak,]

fit_maboost <- glm(X12~ X2 + X4 + X6 + X8 + X9 + X10 + X11 + X10*X11 + X5*X7 + X6*X7 ,data=train, family=gaussian )
prediksi.mb <- predict(fit_maboost, test)
prediksi.test <-ifelse(prediksi.mb>0.4,1,0)
head(prediksi.mb)

confusionMatrix(factor(test$X12), factor(prediksi.test), positive="1")

write.csv(train, file = "Example.csv")

cor(data)
#Membandingkan tiga metode
data1<-data
n<-100
data.accuracy<-cbind(asli=c(1:n),rus=c(1:n),ros=c(1:n))
data.Sensitivity<-cbind(asli=c(1:n),rus=c(1:n),ros=c(1:n))
data.Specificity<-cbind(asli=c(1:n),rus=c(1:n),ros=c(1:n))

for (j in 1:n)
{
  p<-1000+j
  set.seed(p)
  folds <- cvFolds(NROW(data1), K=5)
  data1$pred.asli <- rep(0,nrow(data1))
  data1$pred.rus <- rep(0,nrow(data1))
  data1$pred.ros<- rep(0,nrow(data1))
  
  #Perform 5 fold cross validation
  for(k in 1:5)
  {
    data.training <- data1[folds$subsets[folds$which != k], ] #Set the training set
    data.testing <- data1[folds$subsets[folds$which == k], ] #Set the validation set
    
    #Asli
    fit_maboost <- maboost(X12~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11
                           ,data=data.training
                           ,iter = 10 
                           ,verbose = TRUE
    )
    prediksi.mb <- predict(fit_maboost, data.testing, type = "class")  
    data1[folds$subsets[folds$which == k], ]$pred.asli <- prediksi.mb
    
    #RUS
    down_train <-downSample(x = data.training[, c(2:12)],y = as.factor(data.training$X12))
    model.maboost <- maboost(Class~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11
                             , data=down_train
                             , iter = 10 
                             ,verbose = TRUE)
    prediksi.mb <- predict(model.maboost, data.testing)
    data1[folds$subsets[folds$which == k], ]$pred.rus <- prediksi.mb
    
    #ROS
    up_train <-upSample(x = data.training[, c(2:12)],y = as.factor(data.training$X12))
    model.maboost <- maboost(Class~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11
                             ,data=up_train
                             ,iter = 10 
                             ,verbose = TRUE)
    prediksi.mb <- predict(model.maboost, data.testing)
    data1[folds$subsets[folds$which == k], ]$pred.ros <- prediksi.mb
    
  }
  
  
  data1$pred.asli1<-ifelse(data1$pred.asli==2,1,0)
  data.1<-confusionMatrix(factor(data1$X12), factor(data1$pred.asli1), positive="1")
  test<-data.1$overall
  test2<-data.frame(test)
  data1$pred.asli
  data.accuracy[j,1]<-test2[1,1]
  test<-data.1$byClass
  test2<-data.frame(test)
  data.Sensitivity[j,1]<-test2[1,1]
  data.Specificity[j,1]<-test2[2,1]
  
  data1$pred.rus1<-ifelse(data1$pred.rus==2,1,0)
  data.1<-confusionMatrix(factor(data1$X12), factor(data1$pred.rus1), positive="1")
  test<-data.1$overall
  test2<-data.frame(test)
  data.accuracy[j,2]<-test2[1,1]
  test<-data.1$byClass
  test2<-data.frame(test)
  data.Sensitivity[j,2]<-test2[1,1]
  data.Specificity[j,2]<-test2[2,1]
  
  data1$pred.ros1<-ifelse(data1$pred.ros==2,1,0)
  data.1<-confusionMatrix(factor(data1$X12), factor(data1$pred.ros1), positive="1")
  test<-data.1$overall
  test2<-data.frame(test)
  data.accuracy[j,3]<-test2[1,1]
  test<-data.1$byClass
  test2<-data.frame(test)
  data.Sensitivity[j,3]<-test2[1,1]
  data.Specificity[j,3]<-test2[2,1]
  
}

write.csv(data.accuracy, file = "data_acc.csv")
write.csv(data.Sensitivity, file = "data_sen.csv")
write.csv(data.Specificity, file = "data_spe.csv")

#Membandingkan dua metode
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

data1 <- disk(data)
n<-10
data.accuracy<-cbind(asli=c(1:n),disk=c(1:n))
data.Sensitivity<-cbind(asli=c(1:n),disk=c(1:n))
data.Specificity<-cbind(asli=c(1:n),disk=c(1:n))

for (j in 1:n)
{
  p<-1000+j
  set.seed(p)
  folds <- cvFolds(NROW(data1), K=5)
  data1$pred.asli <- rep(0,nrow(data1))
  data1$pred.disk <- rep(0,nrow(data1))
  
  #Perform 5 fold cross validation
  for(k in 1:5)
  {
    data.training <- data1[folds$subsets[folds$which != k], ] #Set the training set
    data.testing <- data1[folds$subsets[folds$which == k], ] #Set the validation set
    
    #Asli
    fit_maboost <- maboost(X12~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11
                           ,data=data.training
                           ,iter = 100 
                           ,verbose = TRUE
    )
    prediksi.mb <- predict(fit_maboost, data.testing, type = "class")  
    data1[folds$subsets[folds$which == k], ]$pred.asli <- prediksi.mb
    
    #Disk
    fit_maboost <- maboost(X12~ x1_p+x2_p+x3_p+x4_p+x5_p+x6_p+x7_p+x8_p+x9_p+x10_p+x11_p
                           , data=data.training
                           , iter = 100
                           ,verbose = TRUE)
    
    prediksi.mb <- predict(fit_maboost, data.testing)
    data1[folds$subsets[folds$which == k], ]$pred.disk <- prediksi.mb
  }
  
  
  data1$pred.asli1<-ifelse(data1$pred.asli==2,1,0)
  data.1<-confusionMatrix(factor(data1$X12), factor(data1$pred.asli1), positive="1")
  test<-data.1$overall
  test2<-data.frame(test)
  data1$pred.asli
  data.accuracy[j,1]<-test2[1,1]
  test<-data.1$byClass
  test2<-data.frame(test)
  data.Sensitivity[j,1]<-test2[1,1]
  data.Specificity[j,1]<-test2[2,1]
  
  data1$pred.disk1<-ifelse(data1$pred.disk==2,1,0)
  data.1<-confusionMatrix(factor(data1$X12), factor(data1$pred.disk1), positive="1")
  test<-data.1$overall
  test2<-data.frame(test)
  data.accuracy[j,2]<-test2[1,1]
  test<-data.1$byClass
  test2<-data.frame(test)
  data.Sensitivity[j,2]<-test2[1,1]
  data.Specificity[j,2]<-test2[2,1]
  
  
}

write.csv(data.accuracy, file = "data_acc_v2.csv")
write.csv(data.Sensitivity, file = "data_sen_v2.csv")
write.csv(data.Specificity, file = "data_spe_v2.csv")


train.data<-read.delim("clipboard")
test.data<-read.delim("clipboard")


library(cvTools)
library(maboost)
library(caret)
library(discretization)

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

train.data1<-disk(train.data)
test.data1<-disk(test.data)

acak <- sample(1:nrow(train.data1), 45000)
train <- train.data1[acak,]
test <-train.data1[-acak,]

fit_maboost <- maboost(X12~ x1_p+x2_p+x3_p+x4_p+x5_p+x6_p+x7_p+x8_p+x9_p+x10_p+x11_p
                       , data=train
                       , iter = 1000
                       ,verbose = TRUE)

prediksi.maboost <- predict(fit_maboost, test)
confusionMatrix(factor(test$X12), factor(prediksi.maboost), positive="1")
fit_maboost
prediksi.maboost <- predict(fit_maboost, test.data1)
prediksi <-cbind(ID=test.data1$ID,prediksi=prediksi.mb)
write.csv(prediksi.maboost, file = "prediksi.maboost.csv")