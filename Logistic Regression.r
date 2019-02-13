#Напишите функцию, которая принимает на вход два набора данных. Первый dataframe, как и в предыдущей задаче, содержит 
#информацию об уже осмотренном багаже. Второй набор данных — это информация о новом багаже, который сканируется прямо сейчас. 
#Используя первый набор данных, обучите регрессионную модель различать запрещенный и разрешенный багаж. ваша функция принимает 
#два набора данных и возвращает имя пассажира с наиболее подозрительным багажом.

most_suspicious <- function(test_data, data_for_prediction) {
  fit <- glm(is_prohibited ~ ., test_data, family = "binomial")
  data_for_prediction$probability <- predict(fit, data_for_prediction, type = "response")
  max_prob <- which(data_for_prediction$probability == max(data_for_prediction$probability))
  return(as.vector(data_for_prediction[max_prob, "passangers"]))
}

# Напишите функцию get_features , которая получает на вход набор данных о багаже. Строит логистическую регрессию, где зависимая 
# переменная  - являлся ли багаж запрещенным, а предикторы - остальные переменные, и возвращает вектор с названиями 
# статистически значимых переменных (p < 0.05) (в модели без взаимодействия). Если в данных нет значимых предикторов, функция 
# возвращает строку с сообщением  "Prediction makes no sense".

get_features <- function(x) {
  fit <- glm(is_prohibited ~ ., x, family = "binomial")
  result <- anova(fit, test = "Chisq")
  p <- result$`Pr(>Chi)`
  z <- which(p < 0.05)
    if (length(z) >= 1) {
    output <- rownames(result)[z]
  } else {
    output <- "Prediction makes no sense"
  }
  return(output)
}

# По имеющимся данным в переменной admit постройте логистическую регрессионную модель, предсказывающую результат поступления 
# по престижности учебного заведения среднего образования и результатов GPA (переменная gpa) с учётом их взаимодействия. 
# Примените эту модель к той части данных, где результат поступления неизвестен.
# Ответом в задаче будет предсказанное моделью число поступивших из тех, для кого результат поступления был неизвестен. 
# Считаем человека поступившим, когда вероятность его поступления не меньше 0.4.

df <- read.csv("data.csv")
df$rank <- factor(df$rank)
df$admit <- factor(df$admit)
fit <- glm(admit ~ rank*gpa, na.exclude(df), family = "binomial")
data_for_prediction <- df[is.na(df$admit),]
data_for_prediction$prediction <- predict(fit, data_for_prediction, type = "response")
data_for_prediction$enrollment <- ifelse(data_for_prediction$prediction >= 0.4, 1, 0)
sum(data_for_prediction$enrollment)

#Напишите функцию normality_by, которая принимает на вход dataframe c тремя переменными. Первая переменная количественная, 
#вторая и третья имеют две градации и разбивают наши наблюдения на группы. Функция должна проверять распределение на нормальность 
#в каждой получившейся группе и возвращать dataframe с результатами применения теста shapiro.test 

normality_by <- function (test_data) {
  names(test_data)[1] <- c("p_value")
  test_data %>%
    group_by_at(names(test_data)[2:3]) %>%
    summarise(p_value = shapiro.test(p_value)$p.value)
}

#альтернативно также можно:

normality_by <- function (test_data) {
  data <- aggregate(test_data[,1] ~ test_data[,2] + test_data[,3], 
                    FUN = function(i) shapiro.test(i)$p.value)
  names(data) <- c(names(test_data)[2:3], "p_value")
  return(data)
}

# Написать функцию centered, которая получает на вход датафрейм и имена переменных, которые необходимо центрировать. 
#Функция должна возвращать этот же датафрейм, только с центрированными указанными переменными.

centered <- function(test_data, var_names) {
  if (length(var_names) > 1) {
  cent <- sapply(test_data[, var_names], function(i) i-mean(i))
  test_data[, var_names] <- cent
} else {
  cent <- data[, var_names] - mean(data[, var_names])
  test_data[, var_names] <- cent
}
  return(test_data)
}

# Напишите функцию normality_test, которая получает на вход dataframe с произвольным количеством переменных разных типов 
# (количественные, строки, факторы) и проверяет нормальность распределения количественных переменных. Функция должна возвращать 
# вектор значений p-уровней значимости теста shapiro.test для каждой количественной переменной.

normality_test <- function(dataset) {
  numeric_col <- dataset[, unlist(lapply(dataset, is.numeric))]
  shapiro_p <- apply(numeric_col, 2, function(i) shapiro.test(i)$p.value)
  return(shapiro_p)
}

#или:

normality_test <- function(dataset) {
  numeric_col <- sapply(dataset, is.numeric)
  shapiro_p <- sapply(dataset[numeric_col], function(x) shapiro.test(x)$p.value) 
  return(shapiro_p)
}

# Напишите функцию get_coefficients, которая получает на вход dataframe с двумя переменными x ( фактор с произвольным числом 
#градаций) и y ( фактор с двумя градациями ). Функция строит логистическую модель, где y — зависимая переменная, а 
#x — независимая, и возвращает вектор со значением экспоненты коэффициентов модели. 

get_coefficients <- function(data) {
  fit <- glm(y ~ x, data, family = "binomial")
  return(exp(fit$coefficients))
}