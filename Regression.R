# Linear REGRESSION

data("cars")
head(cars)
str(cars)
plot(cars) #산점도 그리기
cor(cars$speed,cars$dist) # x, y 공분산

lm1<-lm(dist~speed,cars) # y ~ x 회귀 모형
summary(lm1) # dist = 3.93 * speed - 17.58
plot(cars$speed, cars$dist)

lm2<-lm(dist~0+speed,cars) # y ~ x
summary(lm2) # dist = 2.9 * speed
plot(cars$speed, cars$dist)
abline(lm1, col=2) # 회귀선
abline(lm2,col=3) # 회귀선

plot(lm1,1)
plot(lm2,2)
shapiro.test(lm1$residuals)
plot(lm1,3)
plot(lm1,4)
library(lmtest)
dwtest(lm1)

#EX2
data("faithful")
head(faithful)
str(faithful)
plot(faithful)
cor(faithful$waiting,faithful$eruptions) # 상관계수, https://ordo.tistory.com/21

lm3<-lm(eruptions ~ waiting, faithful)
summary(lm3)
plot(faithful$waiting,faithful$eruptions)
lm4<-lm(eruptions~0+waiting,faithful) # y ~ x
summary(lm4)
abline(lm3,col=2) # eruptions = 0.075 * waiting -1.87
abline(lm4,col=3) 

plot(lm3,1)
plot(lm3,2)
shapiro.test(lm3$residuals)
plot(lm3,3)
plot(lm3,4)
library(lmtest)
dwtest(lm3)

#prediction
newdata <-data.frame(waiting=50)
newdata
predict(lm3,newdata)
predict(lm3,newdata, interval = "confidence")

#EX3
library(MASS)
data(cats)
head(cats)
str(cats)
cats<-data.frame(cats)
cats
cor(cats$Bwt,cats$Hwt)

lm5<-lm(Hwt~Bwt, cats)
summary(lm5)
plot(cats$Hwt~cats$Bwt, main = "Kitty Cat Plot", pch=19, col = "blue")
abline(lm5,col=2 , lwd=2)

shapiro.test(lm5$residuals)
dwtest(lm5)

#더미변수
# 성별로 데이터 분할#################################################
library(dplyr)
cats$Sex <- as.factor(cats$Sex)
female <-cats %>%filter(Sex=="F")
male <-cats %>%filter(Sex=="M")

lm6<-lm(Hwt~Bwt, female)
summary(lm6)

lm7<-lm(Hwt~Bwt, male)
summary(lm7)

abline(lm6,col=3 , lwd=2)
abline(lm7,col=4 , lwd=2)
#인종별 점수 비교
hsb2 <- read.csv("https://stats.idre.ucla.edu/stat/data/hsb2.csv")
table(hsb2$race)
str(hsb2)
hsb2$racef <- factor(hsb2$race)
lm1 <-lm(read ~ racef, hsb2) #더미변수 
summary(lm1)
#여성 점수
lm2 <-lm(read ~ female, hsb2)
summary(lm2)
