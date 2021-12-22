#Naive Bayse
data1 <-read.csv('~/Desktop/수업자료/2021-2학기/R/mushroom.csv')
str(data1)

#factor로 바꾸기
data1[,1]<-factor(data1[,1])
#전제 변환
for (i in (1:ncol(data1))) {data1[,i]<-factor(data1[,i])}
str(data1)

#install.packages('e1071',dependencies = TRUE)
library(e1071)
model1 <-naiveBayes(class ~. ,data=data1)
predict1 = predict(model1, data1)
t1<-table(predict1,data1$class) #confusion matrix - testdata!
sum(diag(t1)/sum(t1)) #accuracy
t1
#7:3 데이터 분할
s1<-sample(1:nrow(data1), size=round(0.7*nrow(data1)),replace=F)
data1_train <-data1[s1,]
data1_test<-data1[-s1,]
model2<-naiveBayes(class~.,data1_train) #train
pred2<-predict(model2,data1_test) #test
t2<- table(pred2,data1_test$class) #testdata!
sum(diag(t2)/sum(t2))

#ex1
addata <- read.csv('~/Desktop/수업자료/2021-2학기/R/admission.csv')
str(addata)

addata$admit<-as.factor(addata$admit)
addata$rank<-as.factor(addata$rank)
str(addata)
s2 <-sample(1:nrow(addata),size=round(0.8*nrow(addata)),replace = F)
ad_train <-addata[s2,]
ad_test <-addata[-s2,]
model_ad <- naiveBayes(rank~., ad_train)
pre_ad <-predict(model_ad,ad_test)
t3<-table(pre_ad,ad_test$rank)
t3
sum(diag(t3)/sum(t3))

#ex2
ddata=read.csv('~/Desktop/수업자료/2021-2학기/R/diabetes.csv')
str(ddata)
ddata$Outcome<-as.factor(ddata$Outcome)

set.seed(1234)
s3 <-sample(1:nrow(ddata),size=round(0.8*nrow(ddata)),replace = F)
d_train <-ddata[s3,]
d_test <-ddata[-s3,]
table(d_train$Outcome)
table(d_test$Outcome)

mylogit <-glm(admit ~., family=binomial, trainData) #train

#forward selection
Fitstart = glm(Outcome ~1, family=binomial,  ddata)
summary(Fitstart)
FitAll = glm(Outcome~.,family=binomial, ddata)
step(Fitstart,direction="forward", scope = formula(FitAll))
#stepwise
dmodel<-step(FitAll,direction="both")
dmodel
dmodel<-naiveBayes(Outcome~.,d_train)
pred3<-predict(dmodel,d_test)
t3<- table(pred3,d_test$Outcome)
t3
sum(diag(t3)/sum(t3))

mylogit <-glm(Outcome ~ Glucose + BMI + Pregnancies + DiabetesPedigreeFunction + BloodPressure + Age + Insulin, family=binomial, d_train) #train
summary(mylogit)

p1<-predict(mylogit, d_test, type='response') #test

predi1 <- ifelse(p1>0.4, 1,0)
#예측값이 0.5가 넘으면 1 로 (즉 admit로), 아니면 0으로 (못받은 것)으로 하라는 함수 - threshold 지정
length(predi1)
length(d_test$Outcome)
tab1 <- table (PREDICTION=predi1, ACTUAL=d_test$Outcome)
tab1  #Confusion matrix
sum(diag(tab1))/sum(tab1)  #accuracy 계산

