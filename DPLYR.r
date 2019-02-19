# Написать функцию descriptive_stats, которая рассчитывает основные описательные статистики в каждой группе наблюдений.
# Функция должна возвращать dataframe с описательными статистиками и количеством NA, рассчитанными в каждой группе: количеств 
# наблюдений, среднее значение, стандартное отклонение, медиана, первый квартиль, третий квартиль, число пропущенных значений.

library(lazyeval)
descriptive_stats <- function(x) {
  num_var <- names(which(sapply(x, is.numeric)))
  factor_var <- names(which(sapply(x, is.factor)))
  x %>% 
    group_by_(.dots = factor_var) %>% 
    summarise_(n = interp(~n(), var = as.name(num_var)),
               mean = interp(~mean(var, na.rm = T), var = as.name(num_var)),
               sd = interp(~sd(var, na.rm = T), var = as.name(num_var)),
               median = interp(~median(var, na.rm = T), var = as.name(num_var)),
               first_quartile = interp(~quantile(var, na.rm = T)[2], var = as.name(num_var)),
               third_quartile = interp(~quantile(var, na.rm = T)[4], var = as.name(num_var)),
               na_values = interp(~sum(is.na(var)), var = as.name(num_var)))
            }

# Напишите функцию, to_factors, которая получает на вход dataframe  с произвольным числом количественных переменных и вектор с 
# номерами колонок, которые нужно перевести в фактор. Для перевода числовых колонок в фактор будем использовать следующий принцип, 
# если наблюдение больше среднего всей переменной то 1, иначе 0.

to_factors <- function(test_data, factors){
  test_data %>% 
    mutate_at(factors, funs(factor(ifelse(. > mean(.), 1, 0))))
}

# avianHabitat. Произведите следующий подсчёт по всем комбинациям места (Site) и наблюдателя (Observer). Интересующая нас 
# статистика -- количество тех замеров, где обнаруживается хотя бы один экземпляр вида. Соберите эту статистику по всем видам. 
# У вас получится таблица, в которой тройке вида (место M, наблюдатель N, вид P) соответствует число замеров в M, произведенных N, 
# где обнаружен хотя бы один P.

library(tidyr)
library(dplyr)
library(data.table)
library(stringr)

data <- read.csv("https://raw.githubusercontent.com/avynychenko/R_studing_tasks/master/Data%20for%20tasks/avianHabitat.csv")

data$Site <- as.character(data$Site)
data$site_name <- factor(str_replace(data$Site, "[:digit:]+", ""))

ht_vars <- names(data)[str_detect(names(data), ".Ht$")]
ht_new_vars <- str_replace(ht_vars, "Ht$", "")
setnames(data, old = ht_vars, new = ht_new_vars)

new_df <- select(data, site_name, 2, ht_new_vars)

gathered_data <- gather(new_df, plants, height , -(1:2))

result <- gathered_data %>% 
  filter(height > 0) %>% 
  group_by(site_name, Observer, plants) %>% 
  summarise(n = n())


# Напишите функцию find_outliers, которая получает на вход dataframe с одной количественной переменной и произвольным числом 
# факторных переменных. Факторные переменные разбивают все наши наблюдения на определенное число групп. Итак, ваша задача — 
# создать в данных новую числовую переменную is_outlier, которая будет принимать значение 1, если наблюдение в этой строке 
# является выбросом в своей группе, и 0, если не является.

find_outliers <- function(x) {
  factors <- names(which(sapply(x, is.factor)))
  num_col <- names(which(sapply(x, is.numeric)))
    x %>% 
    group_by_(.dots = factors) %>% 
    mutate_(is_outlier =  interp(~ifelse(var > (mean(var) + 2*sd(var)) | 
                                 var < (mean(var) - 2*sd(var)), 1, 0), 
                                 var = as.name(num_col)))
}

# #  Написать функцию get_id, которая получает на вход лист, в котором 7 таблиц, в каждой из которых id и температура.
# Функция, должна вернуть новый датафрэйм, в котором будут две переменные id и mean_temp - среднее значение температуры за 
# неделю только тех пациентов, которые посетили все семь приемов, то есть id таких пациентов присутствует в каждом из семи 
# датафреймов.

get_id <- function(x) {
  data <- bind_rows(x)
  result <- data %>% 
    group_by(id) %>% 
    filter(n() == 7) %>% 
    summarise(mean_temp = mean(temp))
  return(result)
}

# Из данных mtcars отберите только четыре переменные: mpg, hp, am, vs. Оставьте только те наблюдения, для которых значения 
# mpg > 14 и hp > 100. Отсортируйте получившиеся данные по убыванию переменной mpg и возьмите только первые 10 строчек. 
# Переменную mpg переименуйте в Miles per gallon, а переменную hp в  Gross horsepower 

my_df <- mtcars %>% 
  select(mpg, am, vs, hp) %>% 
  filter(mpg > 14, hp > 100) %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  rename("Miles per gallon" = mpg, "Gross horsepower" = hp)

  # Написать функцию, которая получает на вход dataframe  с произвольным числом переменных разных типов. На первом этапе функция 
  #должна выполнить предобработку числовых переменных - центрирование всех переменных. После рассчитать значение натурального 
  # логарифма каждого наблюдения (функция log) и вернуть новый dataframe. 

  log_transform <- function(test_data){
  test_data %>% 
    mutate_if(is.numeric, funs(log((. - min(.))/(max(.) - min(.)) + 1)))
}

# diamonds.Создайте новый dataframe с именем high_price, в котором будут хранится только 10 самых дорогих бриллиантов каждого 
# цвета. Также в итоговом datafrmae должны храниться только две переменные color и price. 

high_price <- diamonds %>% 
  select(color, price) %>% 
  group_by(color) %>% 
  arrange(desc(price)) %>% 
  slice(1:10) 

# В переменную d сохраните только нeчетные строчки исходных данных diamonds. 

d <- filter(diamonds, seq(nrow(diamonds)) %% 2 == 1)


