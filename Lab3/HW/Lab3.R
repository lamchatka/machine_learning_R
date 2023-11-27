# EX 1
# Ваша задача в переменную blond_men сохранить долю блондинов (Blond) 
# от общего числа кареглазых мужчин

View(HairEyeColor)

# Команда, которая позволяет посмотреть, какие есть измерения в этой таблице
dimnames(HairEyeColor)

# $Hair [1] "Black" "Brown" "Red" "Blond", $Eye [1] "Brown" "Blue"  "Hazel" "Green", $Sex [1] "Male"   "Female"

# данные только о мужчинах
HairEyeColor[,,'Male']

# ДОЛЖНА ВЕРНУТЬСЯ ТАБЛИЦА, А НЕ ВЕКТОР!!!
male  <- HairEyeColor[, , 'Male']

# https://www.codecamp.ru/blog/r-prop-table/
# margin 2 - делит каждое отдельное значение на суммы столбцов:
# 1-й столбец: 32+53+10+3=98 32/98, 53/98, 10/98, 3/98
tbl <- prop.table(male, 2)
result <-  tbl['Blond', 'Brown']
result

# EX 2
# Посчитайте число голубоглазых женщин в наборе данных HairEyeColor
women <- HairEyeColor[, 'Blue', 'Female']
women
sum(women) # 114

# EX 3
# Постройте столбчатую диаграмму распределения цвета глаз
# по цвету волос только у женщин из таблицы HairEyeColor.
# По оси X должен идти цвет волос, цвет столбиков должен отражать цвет глаз. 
# По оси Y - количество наблюдений.

library("ggplot2")
mydata <- as.data.frame(HairEyeColor[,,'Female'])
ggplot(data = mydata, aes(x = Hair , y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))


# fill= Eye, - Заполняет недостающие значения в выбранных столбцах, используя предыдущую запись
# position="dodge" - укрывается от пересекающихся объектов

# EX 4
# На основе таблицы HairEyeColor создайте ещё одну таблицу, 
# в которой хранится информация о распределении цвета глаз 
# у женщин-брюнеток (Hair = 'Black'). 
# Проведите тест равномерности распределения цвета глаз у брюнеток 
# и выведите значение хи-квадрата для этого теста.

tbl2 <- HairEyeColor['Black', , 'Female']
tbl2
chi <- chisq.test(tbl2)
chi
# Обозреваемые значения(начальные данные)
chi$observed
# Среднее значение? (36+9+5+2)/4 = 13
chi$expected

# Здесь в условии нет нулевой гипотезы
#p_value1 <-  chi$p.value
#p_value1
# Вывод: P-значение > уровня значимости (0,05), 
# следовательно выявлено недостаточно оснований для отклонения нулевой гипотезы

# Это равномерное распределение

# X-квадрат
chi$statistic


# EX 5

library("ggplot2")
diamonds <-  diamonds

diamonds$factor_price <- ifelse(diamonds$price >= mean(diamonds$price), 1, 0) 
diamonds$factor_carat <- ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0)


# Гипотеза 0 - цена и караты бриллиантов не взаимосвязаны 
# Гипотеза 1 - цена и караты бриллиантов взаимосвязаны
tbl4 <- table(diamonds$factor_price, diamonds$factor_carat, dnn=c('Factor_price','Factor_carat'))
tbl4
# Тест равномерности распределения
chi2 <-  chisq.test(tbl4)
chi2
p_value2 <-  chi2$p.value
p_value2
chi2$statistic

# Вывод: P-значение < уровня значимости (0,05), следовательно мы отклоняем нулевую гипотезу
# и считаем истинной альтернативную гипотезу

main_stat <-  chi2$statistic
main_stat


# EX 6
# При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа коробки передач (am) 
# и типа двигателя (vs) в данных mtcars. Результат выполнения критерия сохраните в переменную. 
# Получившийся p - уровень значимости сохраните в переменную fisher_test


# Нулевая гипотеза - am и vs не связаны с друг другом
# Первая гипотеза - am и vs связаны с друг другом
data <- mtcars
# Manual - 0, Auto - 1
data$am <- factor(data$am, labels=c('Manual', 'Auto'))
# S-shaped - 0, V-shaped - 1
data$vs <- factor(data$vs, labels=c('S-shaped', 'V-shaped'))
tbl3 <-  table(data$am, data$vs)
tbl3
fisher_result <- fisher.test(tbl3)
fisher_result
fisher_test <- fisher_result$p.value
fisher_test 

# Вывод: Так как p-значение > 0,05,
# следовательно выявлено недостаточно оснований для отклонения нулевой гипотезы

# EX 7
!!!!!!!!!!!!
# Тест Стъюдента

correct_data <- subset(ToothGrowth, supp=='OJ' & dose==0.5 | supp=='VC' & dose==2)   
t_stat <- t.test(len ~ supp, correct_data)
t_stat
!!!!!!!!



# EX 8

dataset <-  read.table("dataset_11504_15.txt")
# если дисперсии значимо не отличаются (с уровнем 0.05)
bartlett <-  bartlett.test(V1  ~ V2, dataset) # Тест Бартлетта
bartlett$p.value

if (bartlett$p.value >= 0.05){
  # Тест Стъюдента
  test <- t.test(V1  ~ V2, dataset) 
  test$p.value
}else {
  # Критерий Уилкоксона-Манна-Уитни 
  w <- wilcox.test(V1 ~ V2, dataset)
  w$p.value
}











