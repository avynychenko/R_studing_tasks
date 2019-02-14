# Напишите функцию purchases.median.order.price, у которой один аргумент: purchases, и которая возвращает медианную стоимость 
# заказа (число).

purchases.median.order.price <- function(purchases) {
  result <- purchases[quantity > 0, .(price_total = sum(quantity*price)), 
            by = ordernumber][, .(purchases = median(price_total))]
  return(as.numeric(result))
}

# Напишите функцию filter.expensive.available, которая принимает на вход products (объект типа data.table) и вектор названий 
# брендов, и возвращает только те строчки, которые соответствуют товарам, цена которых больше или равна 5000 рублей, доступны на 
# складе, и принадлежат одному из переданных брендов. 

filter.expensive.available <- function(products, brands) {
  products[(price/100 >= 5000 & available == T &
                     brand %in% brands)]
}

# Создайте функцию ordered.short.purchase.data, которая будет принимать purchases, объект data.table, и возвращать таблицу только 
# со столбцами с номером заказа и ID продукта. Упорядочите результат по убыванию стоимости купленного товара. Возвраты 
# (записи с отрицательным количеством предметов в позиции) надо удалить.

ordered.short.purchase.data <- function(purchases) {
  purchases[order(price, decreasing = T)][quantity > 0, .(ordernumber, 
                                                          product_id)]
}

