# EX 1
#Используя данные mtcars, рассчитайте средний расход топлива (mpg) 
#для автомобилей c числом лошадиных сил (hp), 
#большим 120 и у которых вес менее 4000 фунтов.
#Получившийся результат (среднее значение) сохраните в переменную result.

mtcars <- mtcars
result = mean(mtcars$mpg[mtcars$hp > 120 & mtcars$wt < 4])
result

# EX 2
#При помощи функции aggregate рассчитайте стандартное отклонение переменной mpg (расход топлива), переменной disp (вместимости двигателя)  у машин с автоматической и ручной коробкой передач. 
#Полученные результаты (результаты выполнения функции aggregate) сохраните в переменную descriptions_stat.

descriptions_stat <-  aggregate(cbind(mpg, disp) ~ am, mtcars, sd)
# cbind(mpg, disp) - для параметра x, объединяет по столбцам, ~ am + vs - объединяет по am и vs , mtcars - параметр data, откуда берем переменные mpg и disp, sd - функция, которая будет применена
descriptions_stat

# EX 3
#Воспользуемся встроенными данными airquality. В новую переменную сохраните subset исходных данных, оставив наблюдения только для месяцев 5, 6 и 7.
#При помощи функции aggregate рассчитайте количество непропущенных наблюдений по переменной Solar.R (солнечная радиация) в 5, 6 и 7 месяце. Для определения количества наблюдений используйте функцию length().

airquality <-  airquality

subset_df <- subset(airquality, Month%in%c(5,6,7)) # получили подмножество датаврейма в 5, 6 и 7 месяце
subset_df$Month <-  factor(subset_df$Month, labels = c("May","June", "Jule"))
result <- aggregate(Solar.R ~ Month, subset_df, length)
# для каждого значения месяца берем значения переменной Solar.R и считаем кол-во

# EX 4
#Примените функцию describeBy к количественным переменным данных airquality, 
#группируя наблюдения по переменной Month.  Чему равен коэффициент асимметрии (skew) переменной Wind в седьмом месяце?

# installation command for package psych - install.packages("psych",dependencies=TRUE)
library(psych)
mat <- describeBy(x = airquality$Wind, group=airquality$Month, mat = TRUE)
mat$group1 <- factor(mat$group1, labels = c("May","June", "Jule", "August", "September"))
subset_mat <- subset(mat, group1 == "Jule")
skew <- subset_mat[c('skew')]
skew

# EX 5
# В переменной my_vector сохранен вектор с пропущенными значениями. Вам нужно создать новый вектор fixed_vector, в котором все пропущенные значения вектора my_vector будут заменены на среднее значение по имеющимся наблюдениям.
# При этом исходный вектор оставьте без изменений!

my_vector <- rnorm(30) 
my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA
my_vector
fixed_vector <- my_vector
fixed_vector[is.na(fixed_vector)] <- mean(fixed_vector, na.rm=TRUE)
# 
fixed_vector

fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = TRUE))

# EX 6 ??????
#При помощи функции ggplot() или boxplot() постройте график boxplot,
#используя встроенные в R данные airquality. 
#По оси x отложите номер месяца, по оси y — значения переменной Temp.
#На графике boxplot отдельными точками отображаются наблюдения, 
#отклоняющиеся от 1 или 3 квартиля больше чем на полтора межквартильных размаха. 
#Сколько таких наблюдений присутствует в июле (месяц №7)?


#boxplot(airquality$Temp ~ airquality$Month, xlab="Month", ylab="Temp")
# boxplot - Ящик с усами по переменной Temp, сгруппированный по месяцу

library(ggplot2) 
airquality$Month <- factor(airquality$Month, labels = c("May","June", "Jule", "August", "September"))
ggplot(airquality,aes(x=as.factor(Month), y=Temp))+ geom_boxplot() + xlab("Кат.Месяц") + ggtitle("My boxplot")
# qqplot - просто делает разметку(рисует координатную плоскость, но ничего не отображает)


# EX 7
#Используя данные mtcars, нужно построить scatterplot с помощью ggplot из ggplot2, 
#по оси x которого будет hp, по оси y – qsec, а цветом отобразить переменную (mpg). 
#Полученный график нужно сохранить в переменную plot1.


# as.factor Делаем категориальную переменную

# geom_point() - Диаграмма рассеяния по двум переменным 
mtcars <- mtcars
plot1 <- ggplot(mtcars, aes(x = hp, y = qsec, col = mpg)) + geom_point()
# через aes всегда указываем переменные, которые будут являться осями)
plot1

# EX 8
# Основываясь на данных iris постройте график Scatterplot (диаграмма рассеивания),
# где по оси X будет отложена переменная Sepal.Length,  
# по оси Y переменная  Sepal.Width. 
#За цвет точек будет отвечать переменная  Species, а за размер точек переменная Petal.Length.

ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species, size = Petal.Length)) + geom_point()

