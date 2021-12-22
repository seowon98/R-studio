str(iris)
iris
iris_sub = subset(iris, Species == 'setosa' | Species == 'versicolor')
iris_sub            
t.test(Petal.Width~Species,iris_sub,paired=TRUE) # p-value < 2.2e-16
t.test(Sepal.Length~Species,iris_sub,paired=TRUE) # p-value = 1.242e-13

# t test 결과 setosa와 versicolor의 Petal.Width, Sepal.Length는 차이가 있다. (귀무가설 기각)