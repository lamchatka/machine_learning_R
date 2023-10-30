# control statements

mydata <- read.csv('evals.csv')



# if

a <- 3

if (a > 0){
  print('positive')
} else {
  print('not positive')
}


if (a > 0){
  print('positive')
} else print('not positive')


if (a > 0){
  print('positive')
} else if (a < 0) {
  print('negative')
} else print('zero')



# ifelse

a <- 10


ifelse(a > 0, 'positive', 'not positive')

a <- c(1, -1)


# for

for (i in 1:100){
  print(i)
}


for (i in 1:nrow(mydata)){
  print(mydata$score[i])
}



# for + if
for (i in 1:nrow(mydata)){
  if (mydata$gender[i] == 'male'){
    print(mydata$score[i])
  }
}



# for + if  VS  ifelse

mydata$quality <- rep(NA, nrow(mydata))

for (i in 1:nrow(mydata)){
  if (mydata$score[i] > 4){
    mydata$quality[i] <- 'good'
  } else mydata$quality[i] <- 'bad'
}



mydata$quality2 <- ifelse(mydata$score > 4, 'good', 'bad')



# while

i <- 1

while(i < 51){
  print(mydata$score[i])
  i <- i+1
}


#HOMEWORK LAB1

# EX 5
for (i in 1:nrow(mtcars)){  # пробегаемся по строчкам
  if(mtcars$carb[i] < 4 | mtcars$cyl[i] > 6) {
    mtcars$new_var[i] <- 1
  } else {
    mtcars$new_var[i] <- 0
  }
}

head(mtcars[c("carb", "cyl", "new_var")])

# ЕШЕ ОДИН ВАРИАНТ
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


