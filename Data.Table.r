# Создайте функцию get.category.ratings, которая будет возвращать суммарный оборот (с учетом скидок) каждой категории , и 
# количество купленных предметов по таблице покупок и таблице принадлежности товара к категории. Если купленный товар принадлежит 
# нескольким категориям, его необходимо учитывать во всех. 

get.category.ratings <- function(purchases, product.category) {
  setkey(purchases, product_id)
  setkey(product.category, product_id)
  data <- merge(product.category, purchases, by = "product_id")
  data[, .(totalcents = sum(totalcents), quantity = sum(quantity)), 
     by = "category_id"]
}

# Напишите функцию, которая будет с помощью := добавлять столбец «price.portion», содержащий процент стоимости товара в заказе, 
# с двумя знаками после запятой (нули после запятой не опускать). Проверяться будет возвращаемая из функции таблица. Тип нового 
# столбца - character (строка). Записи с неположительным количеством товаров убрать перед расчётом.

mark.position.portion <- function(purchases) {
      purchases[quantity > 0][, price.portion := 
          format(round(((ordernumber*quantity*price)/
                        (sum(ordernumber*quantity*price))*100), digits = 2), 
                 nsmall = 2), by = ordernumber]
}

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

