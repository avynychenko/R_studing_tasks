# airquality

# Pассчитайте количество непропущенных наблюдений по переменной Ozone в 7, 8 и 9 месяце. 

airquality$Month <- factor(airquality$Month, labels = c('5', '6', '7', '8', '9'))
airquality_new <- airquality[airquality$Month %in% c(7, 8, 9), ]

result <- aggregate(Ozone ~ Month, airquality_new, length)

#В переменной my_vector сохранен вектор с пропущенными значениями. Вам нужно создать новый вектор fixed_vector, в котором все 
#пропущенные значения вектора my_vector будут заменены на среднее значение по имеющимся наблюдениям.

my_vector <- rnorm(1:30)
my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA
my_vector

z <- is.na(my_vector)  #логический вектор, проверяет где NA
fixed_vector <- replace(my_vector, z, mean(my_vector, na.rm = T))   #заменяет
fixed_vector


# mtcars

# рассчитайте стандартное отклонение переменной hp (лошадиные силы) и переменной disp (вместимости двигателя)  
#у машин с автоматической и ручной коробкой передач

descriptions_stat <- aggregate(cbind(hp, disp) ~ am, mtcars, sd)

# Рассчитайте среднее значение времени разгона (qsec) для автомобилей, число цилиндров (cyl) у которых не равняется 3 и 
#показатель количества миль на галлон топлива (mpg) больше 20.

result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20])

# Создайте новую числовую переменную  new_var в данных mtcars, которая содержит единицы в строчках, если в машине не меньше 
#четырёх карбюраторов (переменная "carb") или больше шести цилиндров (переменная "cyl"). В строчках, в которых условие не 
#выполняется, должны стоять нули.

mtcars$new_var <- ifelse((mtcars$carb >= 4 | mtcars$cyl > 6), 1, 0)

#создать новый dataframe под названием mini_mtcars, в котором будут сохранены только третья, седьмая, десятая, двенадцатая и 
#последняя строчка датафрейма mtcars.

mini_mtcars <- data.frame(mtcars[c(3, 7, 10, 12, nrow(mtcars)), ])



# AirPassengers

# Из встроенных в R данных AirPassengers задача создать переменную good_months и сохранить в нее число пассажиров только в тех 
#месяцах, в которых это число больше, чем показатель в предыдущем месяце.  

AirPassengers <- as.vector(AirPassengers)
y <- ifelse ((AirPassengers[-1] - AirPassengers[-length(AirPassengers)]) > 0, T, F)
good_months <- AirPassengers[-1][y]

# Для встроенных в R данных AirPassengers рассчитайте скользящее среднее с интервалом сглаживания равным 10. 
#Напечатайте получившийся результат (первым значением в выводе должно быть среднее для элементов 1:10, во втором значении - 
#среднее для элементов 2:11 и т.д., в последнем  - среднее для элементов 135 :144) Все полученные значения средних сохраните в 
#переменную moving_average.

x <- cumsum(AirPassengers)[-c(1:10)]
y <- cumsum(AirPassengers)[-c((length(AirPassengers) - 9):length(AirPassengers))]

moving_average <- c(mean(AirPassengers[1:10]), (x-y)/10)
moving_average