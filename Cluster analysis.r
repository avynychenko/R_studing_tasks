# Напишите функцию get_difference, которая получает на вход два аргумента: 
#    - test_data — набор данных с произвольным числом количественных переменных.
#    - n_cluster — число кластеров, которое нужно выделить в данных при помощи иерархической кластеризации. 
# Функция должна вернуть названия переменных, по которым были обнаружен значимые различия между выделенными кластерами (p < 0.05). 

get_difference <-  function(test_data, cluster_number){
  data <- test_data[sapply(test_data, is.numeric)]
  fit <- hclust(dist(data))
  data$cluster <- factor(cutree(fit, cluster_number))
  aov_analysis <- apply(data[,-length(data)], 2, function(i) summary(aov(i ~ cluster, data))[[1]]$'Pr(>F)'[1])
  return(names(aov_analysis[aov_analysis < 0.05]))
}

# Напишите функцию get_pca2, которая принимает на вход dataframe с произвольным числом количественных переменных. Функция 
# должна рассчитать, какое минимальное число главных компонент объясняет больше 90% изменчивости в исходных данных и добавлять 
# значения этих компонент в исходный dataframe в виде новых переменных.

get_pca2 <-  function(test_data){
  data <- test_data[sapply(test_data, is.numeric)]
  cum_prop <- summary(prcomp(data))$importance[3,]
  under90 <- which(cum_prop < 0.90)
  up90 <- which(cum_prop >= 0.90)[1]
  test_data <- cbind(test_data, prcomp(data)$x[, c(under90, up90)])
  return(test_data)
}




