# 1 
# Нулевая гипотеза: между mpg и disp взаимосвязь отсутсвует
# mpg - милли на галлон, disp - смещение
library(coin)

smart_cor <- function(df){
  
  mpg_p_value <- shapiro.test(df$mpg)$p.value
  
  disp_p_value <- shapiro.test(df$disp)$p.value
  
  print(mpg_p_value)
  print(disp_p_value)
  
  if(mpg_p_value < 0.05 | disp_p_value < 0.05){
    
    return(spearman_test(~mpg+disp, df))
  } 
  else {
    
    return(cor.test(df$mpg, df$disp))
  }
}

df <- mtcars

smart_cor(df)
plot(x=df$mpg, y=df$disp)

# Вывод: mpg_p_value > 0.05, то есть вектор с нормальным распределением, 
# disp_p_value < 0.05, то есть вектор отличается от нормального распределения
# Поэтому условие в условном операторе выполняется 
# и функция возвращает коэффициент корреляции Спирмена
# p-value = 0,0000004183 < 0.05 
# => отклоняем нулевую гипотезу об отсутствии взаимосвязи
# => взаимосвязь присутствует



# cor.test(df$mpg, df$disp, method="spearman")$estimate


# 2
tb <- read.table("D:\\Учеба\\Магистратура\\Машинное обучение на языке R\\Lab5\\dataset_11508_12.txt", sep=' ')
View(tb)

fit <- lm(tb$V1 ~ tb$V2)
fit$coefficients
## ?????? Где взять slope

# 3

library("ggplot2")

df <-  subset(diamonds, diamonds$cut %in% c("Ideal") 
              & diamonds$carat == 0.46)
fit <- lm(df$price ~ df$depth, df)
fit_coef <- fit$coefficients
predicted_value <- fit$fitted.values
fit_coef
predicted_value

# 4 

fill_na <- function(x){
  r <- lm(y ~ x_1 + x_2, x)    
  y_full <- x$y
  predict(r, x)
  NAs <- which(is.na(y_full))
  y_full[NAs] <- predict(r, x)[NAs]
  x$y_full <- y_full
  return(x)
}

x <- read.csv("D:\\Учеба\\Магистратура\\Машинное обучение на языке R\\Lab5\\fill_na_test.csv")
fill_na(x)


# 5 
df <- mtcars[, c(1,3:6)]
df
summary(lm(wt ~ mpg + disp + drat + hp, data=df))
# Adjusted R-squared:  0.8374, удаляем статистически незначимый (> 0.05) (drat)
summary(lm(wt ~ mpg + disp + hp, data=df))
# Adjusted R-squared:  0.8428 ,  статистически незначимый (> 0.05) не осталось
# Ответ: Adjusted R-squared:  0.8428
summary(lm(wt ~ mpg + disp + drat, data=df))
# Adjusted R-squared:  0.8236 

# Ответ: Набольший Adjusted R-squared:  0.8428, комбинация: mpg + disp + hp

# 6 