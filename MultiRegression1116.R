mtcars # mpg(연비) 예측하는 다중회귀
# 8, 9열은 더미 변수 (명목변수) 제거
mtcars <-data.frame(mtcars)
mtcars <-mtcars[,-c(8,9)]
str(mtcars)
l1<-lm(mpg~., mtcars)

#forward selection
#하나씩 돌려보기
r1 <- lm(mpg~cyl, mtcars)
summary(r1)
r2 <- lm(mpg~disp, mtcars)
summary(r2)
#....
#forward selection
#한번에!
Fitstart = lm(mpg ~1, mtcars)
summary(Fitstart)
FitAll = lm(mpg~., mtcars)
step(Fitstart,direction="forward", scope = formula(FitAll))
# 최종 formula = mpg ~ wt + cyl + hp

#hp 변수 넣을지 말지 고민일때, 두가지 모형 비교 
fit1<-lm(mpg~wt+cyl+hp,mtcars)
fit2<-lm(mpg~wt+cyl,mtcars)
anova(fit1,fit2) #Pr(>F) 가 p-value. 이 값이 0.05보다 작으면 두 집단 간 통계적으로 유의미한 차이가 있음

#ex1
library(MASS) # 출산아 체중 예측, 있고 없고 0/1는 factor로 변환
str(birthwt)

birthwt <-birthwt[,-1] # low 제거
birthwt$ftv = factor(birthwt$ftv>0) # 편중 데이터 두 개로 범주화
birthwt$smoke= factor(birthwt$smoke)
birthwt$ptl= factor(birthwt$ptl)
birthwt$ht= factor(birthwt$ht)
birthwt$ui= factor(birthwt$ui)

str(birthwt)  

forw = lm(bwt~1, birthwt)
fitall=lm(bwt~., birthwt)
step(forw , direction="forward", scope=formula(fitall))
mb <-step(fitall, direction="backward")
mbb<-step(fitall,direction="both")
summary(mb)
#smoke1  # 다른 변수가 동일하다면 담배를 필 수 록 몸무게 -339만큼 덜 나감
#-339.082 

str(survey)
