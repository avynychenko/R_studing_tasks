# При помощи функции ggplot() или boxplot() постройте график boxplot, используя встроенные в R данные airquality. По оси x 
# отложите номер месяца, по оси y — значения переменной Ozone.

airquality <-  airquality
airquality$Month <- factor(airquality$Month, levels = c('5', '6', '7', '8', '9'))
ggplot(airquality, aes(x = Month, y = Ozone)) +
  geom_boxplot(col = "blue")

# Отобразим на графике различия в продажах (переменная sale), в зависимости от года и номера магазина.

my_plot <-  ggplot(sales, aes(date, sale, col = shop)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2,
               position = position_dodge(.5)) +
  stat_summary(fun.y = mean, geom = "point", size = 2, position = position_dodge(.5)) +
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(.5))

# Построить следующий график, чтобы выяснить есть ли различия в бюджетах фильмов разного жанра из года в год. 

my_plot <- ggplot(data, aes(Type, Budget)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ Year)

# Используя данные iris, постройте график плотности для переменной Sepal.Length. Разбейте график на части по переменной Species.

sl_wrap <- ggplot(iris, aes(Sepal.Length)) +
  geom_density() +
  facet_wrap( ~ Species)

# Отобразить взаимосвязь переменных Sepal.Length (ось X) и Petal.Length (ось Y) внутри трех групп по переменной Species. 
# Для этого отобразите цветом значения переменной Species и добавьте линейное сглаживание в каждой группе. Также, перевести
# на русский название осей и легенд  и изменить отображение по осям.

iris_plot <- ggplot(iris, aes(Sepal.Length, Petal.Length, col = Species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(name = "Длина чашелистика", breaks = 4:8, limits = c(4, 8)) +
  scale_y_continuous(name = "Длина лепестка", breaks = 1:7) +
  scale_color_discrete(name = "Вид цветка",
                       labels=c("Ирис щетинистый", "Ирис разноцветный", 
                                "Ирис виргинский"))

# При помощи функции stat_summary постройте график с доверительными интервалами для демонстрации различий в доходах двух магазинов 
# с учетом времени года.

my_plot <- ggplot(sales, aes(shop, income, col = season)) +
  stat_summary(fun.data = mean_cl_boot, position = position_dodge(.2)) 

# Отобразите взаимосвязь между доходом (income) и числом продаж (sale), цветом точек указав номер магазина (shop). Сохраните 
# график в переменную my_plot. Обратите внимание, что линия тренда одна для всех наблюдений.

my_plot <- ggplot(sales, aes(income, sale)) +
  geom_point(aes(col = shop)) +
  geom_smooth()

#   массив mtcars. Нужно построить scatterplot с помощью ggplot из ggplot2, по оси x которого будет mpg, по оси y - disp, а 
# цветом отобразить переменную (hp).

mtcars <- mtcars
plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, col = hp)) +
  geom_point()


# Постройте столбчатую диаграмму распределения цвета глаз по цвету волос только у женщин из 
# таблицы HairEyeColor. По оси X должен идти цвет волос, цвет столбиков должен отражать цвет глаз. По оси Y - количество наблюдений.


library("ggplot2")
mydata <- as.data.frame(HairEyeColor[ , , "Female"])
obj <- ggplot(data = mydata , aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = position_dodge()) +                       
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj

# Постройте scatterplot по данным iris, сохранив его в переменную my_plot : 
# Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание для каждой группы наблюдений по переменной Species


my_plot <- ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm")


  # Напишите функцию resid.norm, которая тестирует распределение остатков от модели на нормальность при помощи функции 
  # shapiro.test и создает гистограмму при помощи функции ggplot() с красной заливкой "red", если распределение остатков 
  # значимо отличается от нормального (p < 0.05), и с зелёной заливкой "green" - если распределение остатков значимо не отличается 
  # от нормального. 
# На вход функция получает регрессионную модель. Функция возвращает переменную, в которой сохранен график ggplot.

resid.norm <- function(x) {
  if (shapiro.test(x$residuals)$p.value < 0.05) {
    myplot <- ggplot(x$model, aes(x$residuals)) +
      geom_histogram(fill = "red")
  } else {
    myplot <- ggplot(x$model, aes(x$residuals)) +
      geom_histogram(fill = "green")
  }
  return(myplot)
}
 
# Постройте график распределения частот переменной color, на котором за цвет заполнения столбиков отвечает переменная cut. 
# Сохраните код графика в переменную obj.

obj <- ggplot(diamonds, aes(x = color, fill = cut)) +
  geom_histogram(stat = "count", position = position_dodge())

#ToothGrowth. Изобразите различия длины зубов морских свинок в различных условиях дозировки и типа потребляемого продукта.
#По оси x - переменная supp. По оси y - переменная len. Цвет ящиков с усами (boxplot) - переменная dose. 

obj <- ggplot(data = ToothGrowth, aes(supp, len, fill = dose)) +
  geom_boxplot()

# При помощи библиотеки ggplot2 визуализируйте распределение переменной Sepal.Length в трех группах в данных Iris. 

plot <- ggplot(iris, aes(Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5)

  # Построить график violin plot для переменной price в каждой группе наблюдений по переменной color. Сохраните результа в 
  # переменную price_violin.

  price_violin <- qplot(x = color, y = price, data = diamonds, geom = I("violin"))