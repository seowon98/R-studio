# Logistic Regression
mydata <-read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(mydata)
mydata$rank<-factor(mydata$rank)
mydata$admit<-as.factor(mydata$admit)
set.seed(1234)

samsize<-floor(0.8*nrow(mydata))
cn<-sample(1:nrow(mydata), size=samsize)
trainData<-mydata[cn,]
testData<-mydata[-cn,]

mylogit <-glm(admit ~., family=binomial, trainData) #train
summary(mylogit)

p1<-predict(mylogit, testData, type='response') #test

predi1 <- ifelse(p1>0.6, 1,0)
#예측값이 0.5가 넘으면 1 로 (즉 admit로), 아니면 0으로 (못받은 것)으로 하라는 함수 - threshold 지정
tab1 <- table (PREDICTION=predi1, ACTUAL=testData$admit)
tab1  #Confusion matrix
sum(diag(tab1))/sum(tab1)  #accuracy 계산


#prediction 객체 생성하기
library(ROCR)
pred <- prediction(ld_result$posterior[,2], admit)

#ROC curve 그리기
#https://freshrimpsushi.github.io/posts/how-to-plot-roc-curve/
#https://dsdoris.medium.com/roc-curve%EC%99%80-auc-%EC%9D%B4%ED%95%B4%ED%95%98%EA%B8%B0-126978d80a9e
pr <- prediction(p1, testData$admit)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main='ROC of Test Data') #곡선과 왼쪽 위의 점 (0,1)(0,1) 이 가깝게 붙는 식으로 나오는 게 좋다
str(prf)

# ex1
str(iris)
table(iris$Species)
subiris= subset(iris, Species == 'setosa' | Species == 'versicolor')
#subiris =subset(iris, Species !='virginica')
str(subiris)
subiris$Species<-factor(subiris$Species)
set.seed(1234)
subiris$Species <- ifelse(subiris$Species == 'setosa', 1,0)

samsize<-floor(0.75*nrow(subiris))
cn<-sample(1:nrow(subiris), size=samsize)
trainData<-subiris[cn,]
testData<-subiris[-cn,]
#train
trainData
m1 =glm(Species~1, family = binomial, data=trainData) # 변수 1개만
mylogit <-glm(Species ~., family=binomial, trainData) # 모든 변수
summary(mylogit)

#변수 선택
step(ml,direction = "forward", scope = formula(mylogit))
step(mylogit,direction = "backward")
#예측 모델
p1<-predict(mylogit, testData, type='response') #test
testData$p <- p1
predi1 <- ifelse(p1>0.5, 1,0)
tab1 <- table (PREDICTION=predi1, ACTUAL=testData$Species)
tab1  #Confusion matrix
sum (diag(tab1))/sum(tab1)  #accuracy 계산

# 최적 모델 - Petal.Length
f <-glm(Species~Petal.Length, family=binomial, data=trainData)

#ex1
library(readxl)
data <-read_excel('~/Desktop/수업자료/2021-2학기/R/멀티호밍OTT.xlsx')
summary(data)
str(data)
data$ 다시보기 <-factor(data$ 다시보기)
data$ 해외 <-factor(data$ 해외)
data$ 실시간 <-factor(data$ 실시간)
data$ 영화 <-factor(data$ 영화)
data$ 오리지널 <-factor(data$ 오리지널)
data$ Multi <-factor(data$ Multi)
str(data)
data[,12]<-factor(data[,12])

logit <-glm(Multi ~., family=binomial, data)

summary(logit)

for (i in (1:ncol(data))) {data[,i]<-factor(data[,i])}
warnings()

data
numeric_vector <- as.numeric(as.character(factor_vector))