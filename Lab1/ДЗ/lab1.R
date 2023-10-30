# EXERCISE 1
home_vector1 <- c(21, 18, 36, 19, 25, 20, 17, 17, 18, 22, 17, 18, 18, 54, 19, 27, 21, 20, 24, 17, 15, 24, 24, 29, 1, 14, 21, 17, 19, 
                  18, 4, 20, 21, 21, 19, 19, 17, 21, 13, 17, 47, 23, 15, 23, 24,16, 17, 72, 24, 22)

mean_home_vector1 <- mean(home_vector1)
standart_deviation <- sd(home_vector1)
home_vector2 <- home_vector1[abs(home_vector1 > mean_home_vector1 + standart_deviation * 1.5) & abs(home_vector1 > mean_home_vector1 - standart_deviation * 1.5)]
home_vector2



# EXERCISE 2
mtcars <-  mtcars
mtcars$even_gear <- ifelse(mtcars$gear %% 2 == 0, 0, 1)
mtcars[c("gear", "even_gear")]


# EXERCISE 3


mtcars$mpg_6 <- ifelse(mtcars$cyl == 6 & mtcars$am == 1, mtcars$mpg, "none")
head(mtcars[c("mpg", "cyl", "am", "mpg_6")])


# EXERCISE 4
mini_mtcars <- mtcars[c(3, 7, 10, 12, nrow(mtcars)), ]
# nrow(mtcars) - последняя строка датафрейма
mini_mtcars

# EX 5
for (i in 1:nrow(mtcars)){  # пробегаемся по строчкам  
  if(mtcars$carb[i] < 4 | mtcars$cyl[i] > 6) { 
    mtcars$new_var[i] <- 1 
  } else { 
    mtcars$new_var[i] <- 0 
  }
} 

head(mtcars[c("carb", "cyl", "new_var")])

# Another solution
mtcars$new_var <- ifelse(mtcars$carb < 4 | mtcars$cyl > 6, 1, 0)

head(mtcars[c("carb", "cyl", "new_var")])


# EX 6
home_vector <- c(20.67, 23.34, 22.65, 17.11, 22.1, 26.32, 20.39, 21.04, 23.78, 31.11, 21.13, 22.44, 23.21, 27.02, 
                 18.64, 20.9, 20.77, 20.0, 21.29, 23.48, 18.47, 25.02, 17.04, 30.97, 12.91, 23.88, 32.95, 8.46, 23.15, 21.05, 20.63, 19.95,
                 17.38, 29.35, 24.43, 23.66, 18.32, 30.13, 19.36, 19.67, 24.23, 20.82, 18.21, 9.91, 21.45, 18.04, 18.31, 17.18, 10.99, 10.06)


if (mean(home_vector) > 20) {
  result <- "My mean is great"
} else {
  result <- "My mean is not so great"
} 

result


#EX 7

library(datasets)

# Загрузка датасета AirPassengers
data(AirPassengers)

# Создание переменной good_months и инициализация её пустым вектором
good_months <- numeric()

# Перебираем месяцы и сравниваем количество пассажиров с предыдущим месяцем
for (i in 2:length(AirPassengers)) {
  if (AirPassengers[i] > AirPassengers[i - 1]) {
    good_months <- c(good_months, AirPassengers[i])
  }
}

print(good_months)

# EX 8
# Загрузка библиотеки
library(datasets)

# Загрузка данных AirPassengers
data(AirPassengers)

# Рассчитывание скользящего среднего с интервалом сглаживания 10
window_size <- 10
moving_average <- numeric(length = length(AirPassengers) - window_size + 1)

for (i in 1:(length(AirPassengers) - window_size + 1)) {
  moving_average[i] <- mean(AirPassengers[i:(i + window_size - 1)])
}

print(moving_average)

