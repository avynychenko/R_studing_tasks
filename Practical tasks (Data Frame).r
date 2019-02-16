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

# Для df avianHabitat создать новые переменные site_name и total_coverage, чтобы определить тот регион, в котором среднее 
# общее покрытие наименьшее.

data <- read.csv("avianHabitat.csv")
data$Site <- as.character(data$Site)
data$site_name <- factor(str_replace(data$Site, "[:digit:]+", ""))
coverage_vars <- names(data)[str_detect(names(data), "^P")]
data$total_coverage <- apply(data[coverage_vars], 1, sum)

data %>% 
  group_by(site_name) %>% 
  summarise(mean = mean(total_coverage)) %>% 
  arrange(mean)
