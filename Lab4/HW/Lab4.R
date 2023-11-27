# 1 ТРЕХФАКТОРНЫЙ ДИСПЕРСИОННЫЙ АНАЛИЗ
# aov - функция для анализа дисперсионной модели
# N + P + K - три фактора(независимые переменные) (N, P, K - типы удобрений)
# summary - выводит определенный набор статистических параметров
# residual() - остатки регресcии, predict() - прогнозные значения,
# coef() - отображает вектор с оценками параметра


fit <- aov(yield ~ N + P + K, data=npk)
summary(fit) # Pr(>F) = N - 0.0192, P - 0.5974,K - 0.0859
# Вывод: Тип N - значимо влияет на урожайность гороха, 
# P и K - значимо не влияют на урожайность гороха

# 2 ОДНОФАКТОРНЫЙ ДИСПЕРСИОННЫЙ АНАЛИЗ
library("ggplot2")

ggplot(iris, aes(x=Species, y=Sepal.Width)) + geom_boxplot() 
# смотрим, как ширина чашелистика влияет на вид
# Более широкий чашелистик у setosa (max=4.4, min=2.3),  
# virginica на втором месте (max=3.8, min=2.2)

# Проводим дисперсионный анализ
fit2 <- aov(Sepal.Width ~ Species, data=iris)
summary(fit2) #  Pr(>F) <2e-16 < 0,05  => вид цветка значимо влияет на ширину чашелистика

# если p < 0,05  - то статистически значимо, отвергаем нулевую гипотезу

# Проводим попарное сравнение, что бы определить какой тип влияет, а какой нет
TukeyHSD(fit2) 
# diff - разница, lwr and upr - доверительные интервалы, p.adj - p-значение

# Вывод после проведения попарного сравнения:
# статистически значимы различия по ширине чашелистика во всех группах

# Если результат статистически значим, 
# это означает, что его получение вследствие случайных 
# событий маловероятно


# 3 Однофакторный дисперсионный анализ
Pillulkin <- read.csv("D:\\Учеба\\Магистратура\\Машинное обучение на языке R\\Lab4\\HW\\Pillulkin.csv")

fit3 <- aov(temperature ~ pill + Error(patient/pill), data=Pillulkin) 
summary(fit3) # Pr(>F)= 0.387 > 0,05 (с другими параметрами - ХЗ)

# построим график с усами с учетом пациентов
ggplot(Pillulkin, aes(x=pill, y=temperature)) + geom_boxplot() + facet_grid(~patient)
# в результате видим, что тип таблетки не вызвал ????? 

# Вывод: Тип таблетки статистически значимо не влияет на температуру пациента

# 4 Двухфакторный дисперсионный анализ с повторными измерениями
fit4 <- aov(temperature ~ doctor*pill + Error(patient/(pill*doctor)), data=Pillulkin)
summary(fit4)
# Вывод: F-value doctor:pill = 0.538 


# ПЕРЕХОДИМ К СОЗДАНИЮ ФУНКЦИЙ

# 5 Создать функцию NA.counter
NA.counter <- function(vector){
  count <-  0
  is_na_vector <- is.na(vector)==TRUE
  for (is_na in is_na_vector){
    if(is_na == TRUE) {
      count <-  count + 1
    }
  }
  
  return(count)
}

vector <-rnorm(100)
vector[1:30] <- NA
vector
NA.counter(vector)

# 6 

vector2 <- c(1, 2, 3, 4, 5, 6, -10, 12)
boxplot(vector2)
outliers.rm <-  function(vector2){ # return new_vector
  q1 <-  quantile(vector2, probs=c(0.25)) # рассчитывает первый квартиль веткора
  q3 <-  quantile(vector2, probs=c(0.75)) # рассчитывает третий квартиль веткора
  new_vector <- c()
  for(i in 1:length(vector2)){
    if(vector2[i] < (q1 - 1.5 * IQR(vector2)) | vector2[i] > (q3 + 1.5 * IQR(vector2))){
      new_vector <- append(new_vector, values=vector2[i])
    }
  }
  return(new_vector)
}

outliers.rm(vector2)



