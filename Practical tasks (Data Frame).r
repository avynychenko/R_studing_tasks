# Написать функцию fix_data, которая получает на вход набор данных, у которого в некоторых значениях числовых переменных добавлен 
# пробел между цифрами. Ваша задача - избавиться от этого пробела и вернуть числовым переменным их числовой тип.

library(tidyverse)
fix_data <- function(d){
  data <- as.data.frame(apply(d, 1:2, function(i) gsub(" " , "", i)), stringsAsFactors = F)
  data <- type_convert(data)
  return(data)
}

# второй вариант, если не использовать пакет tidyverse

fix_data <- function(d){
  data <- sapply(d, function(i) gsub(" " , "", i))
  num <- as.data.frame(apply(data, 2, as.numeric))
  num[is.na(num)] <- data[which(is.na(num))]
  return(num)
}