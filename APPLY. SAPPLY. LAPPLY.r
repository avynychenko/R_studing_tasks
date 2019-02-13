# Напишите функцию smart_lm, которая получает на вход data.frame с произвольным числом количественных переменных. Функция 
# возвращает в виде вектора коэффициенты линейной регрессии построенной только для отобранных предикторов (условие нормальности 
# распределения). Если таких предикторов в данных не оказалось, то функция возвращает предупреждение "There are no normal 
# variables in the data".

smart_lm <- function(x) {
  p_val <- sapply(x[-1], function(i) shapiro.test(i)$p.value)
  predictors <- which(p_val > 0.05)
  if (length(predictors) > 0) {
  zp <- sapply(x[1], as.numeric)
  coef_ <- sapply(x[-1][predictors], as.numeric)
  fit <- lm(zp ~ coef_, x)
  result <- fit$coefficients 
  } else {
  result <- "There are no normal variables in the data"
  }
 return(result)
}

# Напишите функцию na_rm которая заменяет все пропущенные значения в столбцах dataframe на соответствующее среднее значение. 
# То есть все NA в первом столбце заменяются на среднее значение первого столбца (рассчитанного без учета NA). Все NA второго 
# столбца заменяются на среднее значение второго столбца и т.д.  

na_rm <- function(z) {
  as.data.frame(apply(z, 2, function(i) {
    i[is.na(i)] <- mean(i, na.rm = T) 
    return(i)}))}

# Напишите функцию my_names, которая получает на вход  датафрейм и вектор с именами тех генов, для которых мы хотим отобрать 
# наблюдения уровня экспрессии.

my_names <- function (dataset, names){
  names <- paste(names, collapse = "|")
  dataset[sapply(dataset[1], function(i) grepl(names, i)), ]
}

# Напишите функцию positive_sum, которая получает на вход dataframe с произвольным количеством числовых переменных. 
# Основная задача функции - найти сумму положительных значений в каждой переменной и сохранить их в список. 

positive_sum <- function(x) {
  lapply(x, function(i) sum(i[i > 0 & !is.na(i)]))
}



