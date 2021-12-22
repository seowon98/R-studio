a<-rnorm(100,175,2)
b<-rnorm(100,173,2) #n, avg, sd
c<-rnorm(100,175,15)
d<-rnorm(100,173,15)

a # 평균이 175이고 표쥰편차가 2인 100명 데이터 만들기
b # 평균이 175이고 표쥰편차가 2인 100명 데이터 만들기
c
d
mean(a) # 평균이 175

t.test(a,b) # p-value = 1.697e-15 < 0.05 (95%수준 귀무가설(a=b) 기각, 차이 있음)
t.test(c,d) # p-value = 0.1759 > 0.05 (95%수준 귀무가설 채택, 차이 없음)

# Paired t-test
sleep
sleep$extra # column values

t.test(extra ~ group, sleep, paired=TRUE) # p-value = 0.002833 (귀무가설 기각, 차이가 있음)
# t = -4.0621 두번째 약이 더 많이 재움 (더 효과적)

#1-18pg 실습문제1
data1 <-read.csv('~/Desktop/수업자료/2021-2학기/R/ttest1.csv')
data1
before=data1$before
after=data1$after
t.test(before,after,paired = TRUE) #p-value = 0.1982

t.test(data1$before,data1$after,paired = TRUE) 

#1-18pg 실습문제2
data2 <-read.csv('~/Desktop/수업자료/2021-2학기/R/ttest2.csv')

data2
t.test(weight~group,data2,paired=TRUE) # p-value = 0.003136
boxplot(weight~group, data2)

#1-18pg 실습문제3
data3 <- read.csv('~/Desktop/수업자료/2021-2학기/R/bank.csv')
tail(data3)

t.test(R3 ~D, data3)

#dataframe
d<-data.frame(x=c(1,2,3,4,5), y=c(2,4,6,8,10))
d
d$x
d$w <-c("a","b","c","d","e") #열 추가
d
d[1,] #첫번째 열(가로)
d[,2] # 두번째 행(세로)
d[,-2] # 두번째 행 제외

d2<-subset(d,(x>=3)) #3보다 큰 row만
d2
d3<-subset(d,(x!=3))
d3
