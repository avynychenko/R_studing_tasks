# Напишите функцию smart_anova, которая получает на вход dataframe с двумя переменными x и y.Если распределения во всех группах 
#значимо не отличаются от нормального, а дисперсии в группах гомогенны, функция должна сравнить три группы при помощи дисперсионного 
#анализа и вернуть именованный вектор со значением p-value, имя элемента — "ANOVA". Если хотя бы в одной группе распределение 
#значимо отличается от нормального или дисперсии негомогенны, функция сравнивает группы при помощи критерия Краскела — Уоллиса 
#и возвращает именованный вектор со значением p-value, имя вектора  — "KW".

smart_anova <- function(test_data) {
         shapiro <- by(test_data[, 1], test_data[, 2], shapiro.test) 
         bartlett <- bartlett.test(test_data[, 1] ~ test_data[, 2], test_data)
            if (shapiro$A$p.value >= 0.05 &
                shapiro$B$p.value >= 0.05 &
                shapiro$C$p.value >= 0.05 &
                bartlett$p.value >= 0.05) {
              fit <- aov (test_data[, 1] ~ test_data[, 2], test_data)
              p_value <- summary(fit)[[1]]$'Pr(>F)'[1]
              names(p_value) <- c("ANOVA")
            } else {
              fit <- kruskal.test(test_data[, 1] ~ test_data[, 2], test_data)
              p_value <- fit$p.value
              names(p_value) <- c("KW")
            }
         return(p_value)
}

# Нужно провести двухфакторный дисперсионный анализ: влияние факторов doctor, влияние фактора pill и их взаимодействие на temperature. 
#Учесть тот факт, что один и тот же больной принимает разные таблетки, и тот факт, что  один и тот же больной лечится у разных врачей!

df <- read.csv("Pillulkin.csv")
df$patient <- factor(df$patient)
fit2 <- aov(df$temperature ~ doctor*pill + Error(df$patient/(pill*doctor)), df)
summary(fit2)

# Напишите функцию is_multicol, которая получает на вход dataframe произвольного размера с количественными переменными. 
# Функция должна проверять существование строгой мультиколлинеарности, а именно наличие линейной комбинации между предикторами. 
# Функция возвращает имена переменных, между которыми есть линейная зависимость или cобщение "There is no collinearity in the data".

is_multicol <- function(d){
  correlation <- cor(d)
  diag(correlation) <- 0
  logical <- apply(abs(correlation), 1:2, function(i) all.equal(i, 1))
  if(any(logical == TRUE)) {
    result <- dimnames(which(logical == TRUE, arr.ind = T))[[1]]
  } else {
    result <- "There is no collinearity in the data"
  }
  return(result)
}

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

# Напишите функцию high.corr, которая принимает на вход датасет с произвольным числом количественных переменных и возвращает 
# вектор с именами двух переменных с максимальным абсолютным значением коэффициента корреляции.

high.corr <- function(x) {
  correlation <- cor(x)
  diag(correlation) <- 0
  z <- which(abs(correlation) == max(abs(correlation)), arr.ind = TRUE)
  return(dimnames(z)[[1]])
} 

# Напишите функцию fill_na, которая принимает на вход данные с тремя переменными:
# x_1  -  числовой вектор, x_2 - числовой вектор, y - числовой вектор с пропущенными значениями.
# Используя только наблюдения, в которых нет пропущенных значений, мы построим регрессионную модель (без взаимодействий), 
# где  y — зависимая переменная, x_1 и x_2 — независимые переменные. Затем, используя построенную модель, мы заполним 
# пропущенные значения предсказаниями модели.
# Функция должна возвращать dataframe c новой переменной  y_full. Сохраните в нее переменную y, в которой пропущенные значения 
# заполнены предсказанными значениями построенной модели.   

fill_na <- function(x) {
  linear_model <- lm(y ~ x_1 + x_2, na.exclude(x))
  x$y_full <- ifelse (is.na(x$y), predict(linear_model, x[, 1:2]), x$y)
  return(x)
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

# Напишите функцию, которая на вход получает dataframe с двумя количественными переменными, а возвращает стандартизованные 
# коэффициенты для регрессионной модели, в которой первая переменная датафрейма выступает в качестве зависимой, а вторая в 
# качестве независимой.

beta.coef <- function (x) {
  scaled_var <- scale(x)
  fit <- lm(scaled_var[, 1] ~ scaled_var[, 2])
  return(fit$coefficients)
} 

# или:

library(QuantPsyc)
beta.coef <- function (x) {
  return(lm.beta(lm(x[, 1] ~ x[, 2])))
}

# Напишите функцию normality.test, которая получает на вход dataframe с количественными переменными, проверяет распределения 
# каждой переменной на нормальность с помощью функции shapiro.test. Функция должна возвращать вектор с значениями p - value, 
# полученного в результате проверки на нормальность каждой переменной. Названия элементов вектора должны совпадать с названиями 
# переменных. 

normality.test <- function(x) {
  return(apply(x, 2, function (i) shapiro.test(i)$p.value))
}