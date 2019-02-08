
#Дата фрейм attitude -- встроенный массив данных, содержащий рейтинг департаментов одной финансовой компании, составленный 
#сотрудниками. Представьте, что вы хотите устраиваться как раз в эту компанию, и дата фрейм (совершенно случайно!) оказался 
#в вашем распоряжении. 
# Вы решили, что самое главное для вас -- это возможность учиться новому (learning). Возьмите 5 топовых департаментов по этому 
#показателю. Из этого набора вам более всего подойдёт тот департамент, который имеет наибольшую сумму баллов по трём показателям: 
#реакция на жалобы работников (complaints), надбавки в зависимости от результатов работы (raises) и возможность продвижения (advance).


z <- sort(attitude$learning, decreasing = T)
y <- subset(attitude, learning %in% head(z, 5), select = c(complaints, raises, advance))
which.max(rowSums(y))


#добавление датафрейма к действующему нормальному датафрейму под названием avian

avian <- read.csv("https://raw.githubusercontent.com/avynychenko/R_studing_tasks/master/avianHabitat.csv")

data2 <- read.csv2("https://raw.githubusercontent.com/avynychenko/R_studing_tasks/master/avianHabitat2.csv",
                   header = F, sep = ';', dec = ".", skip = 5, na.strings = "Don't remember", comment.char = "%")

x <- as.matrix(data2[1, ])
y <- as.vector(x)
y[length(y)] <- "PB" #в названии этого столбца был пробел после РВ, поэтому этим действием мы его удалили (мне кажется существует
                                                                                                #способ сделать это более красиво)
colnames(data2) <- y 

data_clean <- data2[-1, ]
data_clean$Observer <- "CL"         #добавили недостающую колонку и информацией об исследователе
data_clean <- type.convert(data_clean)   # привели данные к нормальному типу

str(data_clean)

new_df <- rbind(avian, data_clean)   # объединили датафреймы по строкам
summary(new_df)

# подсчёт общего покрытия в новом датафрейме, добавив переменную total_coverage. В качестве ответа указать величину среднего 
#покрытия с точностью до второго знака: X.XX 

new_df$total_coverage <- rowSums(new_df[-(1:4)] [c(T, F)])
mean(new_df$total_coverage)


#задание, по которому я так и не смогла засабмитить потому, что нигде нет соотношения названия растений к данным
#На массиве avianHabitat найдите максимальные высоты по каждому виду растений и отсортируйте эти виды по убыванию, от самого 
#высокого к самому низкому.
avianHabitat <- new_df[-(1:5)][c(T, F)][-17]
avianHabitat$max_height <- apply(avianHabitat, 1, max) #функция считает мах значение в каждой строчке
z <- avianHabitat[order(avianHabitat$max_height, decreasing = T),] #сортирует по убыванию
