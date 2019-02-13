# При помощи функции ggplot() или boxplot() постройте график boxplot, используя встроенные в R данные airquality. По оси x 
# отложите номер месяца, по оси y — значения переменной Ozone.

airquality <-  airquality
airquality$Month <- factor(airquality$Month, levels = c('5', '6', '7', '8', '9'))
ggplot(airquality, aes(x = Month, y = Ozone)) +
  geom_boxplot(col = "blue")


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