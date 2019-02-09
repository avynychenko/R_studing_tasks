# Нужно провести двухфакторный дисперсионный анализ: влияние факторов doctor, влияние фактора pill и их взаимодействие на temperature. 
#Учесть тот факт, что один и тот же больной принимает разные таблетки, и тот факт, что  один и тот же больной лечится у разных врачей!

df <- read.csv("Pillulkin.csv")
df$patient <- factor(df$patient)
fit2 <- aov(df$temperature ~ doctor*pill + Error(df$patient/(pill*doctor)), df)
summary(fit2)



# Провести однофакторный дисперсионный анализ с повторными измерениями: влияние типа таблетки (pill) на температуру (temperature) с 
#учётом испытуемого (patient). Каково p-value для влияния типа таблеток на температуру?

df <- read.csv("Pillulkin.csv")
df$patient <- factor(df$patient)
fit <- aov(temperature ~ pill + Error(patient/pill), df)
summary(fit)


# Воспользуемся встроенными данными npk, иллюстрирующими влияние применения различных удобрений на урожайность гороха (yield). 
#Нашей задачей будет выяснить, существенно ли одновременное применение азота (фактор N) и фосфата (фактор P). 

npk <- npk

fit <- aov(yield ~ N*P, npk)
summary(fit)

# Напишите функцию regr.calc, которая на вход получает dataframe c двумя переменными.
# Если две переменные значимо коррелируют, то функция строит регрессионную модель, где первая переменная - зависимая, 
# вторая - независимая. Затем создает в dataframe новую переменную с назанием fit, где сохраняет предсказанные моделью 
# значения зависимой переменной. В результате функция должна возвращать исходный dataframe с добавленной новой переменной fit.
# Если две переменные значимо не коррелируют, то функция возвращает строчку "There is no sense in prediction"

regr.calc <- function(x) {
  if (cor.test(x[, 1], x[, 2])$p.value < 0.05) {
    model <- lm(x[, 1] ~ x[, 2])
    x$fit <- model$fitted.values
    return(x)
    } else {
    print("There is no sense in prediction")
  }
}

# Напишите функцию filtered.cor которая на вход получает data.frame с  произвольным количеством переменных, рассчитывает 
# коэффициенты корреляции Пирсона между всеми парами количественных переменных и возвращает наибольшее по модулю значение 
# коэффициента корреляции.

filtered.cor <- function(x) {
  numeric_columns <- x[, unlist(lapply(x, is.numeric))]
  corel <- cor(numeric_columns)
  diag(corel) <- 0
  z <- which.max(abs(corel))
  return(corel[z])
}

# Напишите функцию smart_cor, которая получает на вход dataframe с двумя количественными переменными. Проверьте с помощью теста 
# Шапиро-Уилка, что данные в обеих переменных принадлежат нормальному распределению.
# Если хотя бы в одном векторе распределение переменной отличается от нормального (p - value меньше 0.05), то функция должна 
# возвращать коэффициент корреляции Спирмена. (Числовой вектор из одного элемента).
# Если в обоих векторах распределение переменных от нормального значимо не отличается, то функция должна возвращать коэффициент 
# корреляции Пирсона.

smart_cor <- function(x) {
  if (shapiro.test(x[, 1])$p.value >= 0.05 & shapiro.test(x[, 2])$p.value >= 0.05) {
    correlation <-  as.vector(cor.test(x[, 1], x[, 2])$estimate)
  } else {
    correlation <- as.vector(cor.test(x[, 1], x[, 2], method = "spearman")$estimate)
  }
  return(correlation)
}


# diamonds из библиотеки ggplot2. Только для бриллиантов класса Ideal (переменная cut) c числом карат равным 0.46 
# (переменная carat) постройте линейную регрессию, где в качестве зависимой переменной выступает price, в качестве 
# предиктора - переменная  depth.

diamonds <- diamonds
data_for_analysis <- diamonds[diamonds$cut == "Ideal" & diamonds$carat == 0.46, ]
fit <- lm(price ~ depth, data_for_analysis)
fit_coef <- fit$coefficients


# Напишите функцию corr.calc, которая на вход получает data.frame с двумя количественными переменными, рассчитывает 
#коэффициент корреляции Пирсона и возвращает вектор из двух значений: коэффициент корреляции и p - уровень значимости.

corr.calc <- function(x) {
  fit <- cor.test(~ x[, 1] + x[, 2], x)
  return(as.vector(c(fit$estimate, fit$p.value)))
}




