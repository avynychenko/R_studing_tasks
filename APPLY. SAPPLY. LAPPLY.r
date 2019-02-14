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

# Напишите функцию one_sample_t. Ваша функция должна применять одновыборочный t - test к каждой числовой переменной в данных, и 
# сравнивать среднее значение этой переменной с указанным значением среднего в генеральной совокупности (второй аргумент функции).
# Функция должна возвращать список, где каждый элемент это вектор, состоящий из t - значения, числа степеней свобод (df) и 
# значения p - value.

one_sample_t <- function(test_data, general_mean){
  num_col <- test_data[sapply(test_data, is.numeric)]
  lapply(num_col, function(i) c(t.test(i, mu = general_mean)$statistic, 
                                t.test(i, mu = general_mean)$parameter, 
                                t.test(i, mu = general_mean)$p.value))
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

# написать функцию get_p_value, которая получает на вход список (назовем его главным списком), каждый элемент этого списка тоже 
# список - результат выполнения функции shapiro.test (смотри пример normality_tests). Ваша задача из каждого элемента главного 
# списка вытащить только p - value. 

get_p_value <- function(test_list){
  lapply(test_list, function(i) i$p.value)
}

# Напишите функцию get_negative_values, которая получает на вход dataframe произвольного размера. Функция должна для каждой 
# переменной в данных проверять, есть ли в ней отрицательные значения. Если в переменной отрицательных значений нет, то эта 
# переменная нас не интересует, для всех переменных, в которых есть отрицательные значения мы сохраним их в виде списка или 
# матрицы, если число элементов будет одинаковым в каждой переменной.

get_negative_values <- function(x) {
  neg <- sapply(x, function(i) i[i < 0 & !is.na(i)])
  non_zero <- neg[sapply(neg, function(i) length(i) > 0)]
  length_list <- sapply(non_zero, length)
  if (length(unique(length_list)) == 1) {
    result <- sapply(non_zero, as.matrix)
  } else {
    result <- non_zero
  }
return(result)
 }

# Напишите функцию positive_sum, которая получает на вход dataframe с произвольным количеством числовых переменных. 
# Основная задача функции - найти сумму положительных значений в каждой переменной и сохранить их в список. 

positive_sum <- function(x) {
  lapply(x, function(i) sum(i[i > 0 & !is.na(i)]))
}



