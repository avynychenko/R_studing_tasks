# chi-squared

# Данные diamonds из библиотеки ggplot2. Проверить гипотезу о взаимосвязи качества огранки бриллианта (сut) и его цвета (color). 
#В переменную main_stat сохраните значение статистики критерия Хи - квадрат. Обратите внимание, main_stat должен быть вектором из 
#одного элемента.

diamonds <-  diamonds
chi <- chisq.test(diamonds$cut, diamonds$color)
main_stat <- chi$statistic
main_stat <- as.vector(main_stat)

# При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи цены (price) и каратов (carat) бриллиантов. 

diamonds$factor_price <- factor(ifelse(diamonds$price >= mean(diamonds$price), 1, 0), order = T, levels = c("0", "1"))
diamonds$factor_carat <- factor(ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0), order = T, levels = c("0", "1"))
main_stat <- chisq.test(diamonds$factor_carat, diamonds$factor_price)$statistic


# При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа коробки передач (am) и типа двигателя (vs) в данных 
# mtcars. Результат выполнения критерия сохраните в переменную.Получившийся p - уровень значимости сохраните в переменную 
# fisher_test.

mtcars <- mtcars
mtcars$vs <- factor(mtcars$vs, labels = c("V", "S"))
mtcars$am <- factor(mtcars$am, labels = c("A", "M"))
fisher_test <- fisher.test(mtcars$vs, mtcars$am)$p.value
fisher_test


# Сравнение двух групп

# ToothGrowth
#Сравните среднее значение длины зубов свинок, которые потребляли апельсиновый сок (OJ) с дозировкой 0.5 миллиграмм, со средним 
# значением длины зубов свинок, которые потребляли аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма. 

t1 <- ToothGrowth$len[ToothGrowth$dose == 0.5 & ToothGrowth$supp == "OJ"]
t2 <- ToothGrowth$len[ToothGrowth$dose == 2 & ToothGrowth$supp == "VC"]
t_stat <- t.test(t1, t2)$statistic

# Напишите функцию smart_test, которая получает на вход dataframe с двумя номинативными переменными с произвольным числом градаций. 
# Функция должна проверять гипотезу о независимости этих двух переменных при помощи критерия хи - квадрат или точного критерия 
# Фишера. Если хотя бы в одной ячейке таблицы сопряженности двух переменных меньше 5 наблюдений, функция должна рассчитывать 
# точный критерий Фишера и возвращать вектор из одного элемента: получившегося p - уровня значимости.
# Если наблюдений достаточно для расчета хи-квадрат (во всех ячейках больше либо равно 5 наблюдений), тогда функция должна 
# применять критерий хи-квадрат и возвращать вектор из трех элементов: значение хи-квадрат, число степеней свободы,  p-уровня 
# значимости.

smart_test <- function(x) {
  if(all(table(x) >= 5)) {
    result <- c(chisq.test(table(x))$statistic, chisq.test(table(x))$parameter, chisq.test(table(x))$p.value)
  } else {
    result <- c(fisher.test(table(x))$p.value)
  }
  return(result) 
}

# Напишите функцию most_significant, которая получает на вход dataframe с произвольным количеством переменных, где каждая 
# переменная это нуклеотидная последовательность. Функция должна возвращать вектор с названием переменной (или переменных), 
# в которой был получен минимальный p - уровень значимости при проверке гипотезы о равномерном распределении нуклеотидов при 
# помощи критерия хи - квадрат.

most_significant <- function(x) {
  apply(x, 2, function(i) factor(i))
  chi <- apply(x, 2, function (i) chisq.test(table(i))$p.value)
  return(colnames(x)[which(chi == min(chi))])
}

# Функция должна находить ячейку таблицы сопряженности с максимальным  значением стандартизированного остатка и возвращать 
# вектор из двух элементов: название строчки и столбца этой ячейки.

max_resid <- function(x) {
  stdres <- chisq.test(table(x))$stdres
  max_one <- which(stdres == max(stdres), arr.ind = TRUE)
  return(c(rownames(stdres)[max_one[1]], colnames(stdres)[max_one[2]]))
}