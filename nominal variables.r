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


# При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа коробки передач (am) и типа двигателя (vs) в данных mtcars. Результат выполнения критерия сохраните в переменную.Получившийся p - уровень значимости сохраните в переменную fisher_test.

mtcars <- mtcars
mtcars$vs <- factor(mtcars$vs, labels = c("V", "S"))
mtcars$am <- factor(mtcars$am, labels = c("A", "M"))
fisher_test <- fisher.test(mtcars$vs, mtcars$am)$p.value
fisher_test


# Сравнение двух групп

# ToothGrowth
#Сравните среднее значение длины зубов свинок, которые потребляли апельсиновый сок (OJ) с дозировкой 0.5 миллиграмм, со средним значением длины зубов свинок, которые потребляли аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма. 

t1 <- ToothGrowth$len[ToothGrowth$dose == 0.5 & ToothGrowth$supp == "OJ"]
t2 <- ToothGrowth$len[ToothGrowth$dose == 2 & ToothGrowth$supp == "VC"]
t_stat <- t.test(t1, t2)$statistic
