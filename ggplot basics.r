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

