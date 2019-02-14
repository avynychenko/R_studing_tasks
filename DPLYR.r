# Из данных mtcars отберите только четыре переменные: mpg, hp, am, vs. Оставьте только те наблюдения, для которых значения 
# mpg > 14 и hp > 100. Отсортируйте получившиеся данные по убыванию переменной mpg и возьмите только первые 10 строчек. 
# Переменную mpg переименуйте в Miles per gallon, а переменную hp в  Gross horsepower 

my_df <- mtcars %>% 
  select(mpg, am, vs, hp) %>% 
  filter(mpg > 14, hp > 100) %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  rename("Miles per gallon" = mpg, "Gross horsepower" = hp)

# В переменную d сохраните только нeчетные строчки исходных данных diamonds. 

d <- filter(diamonds, seq(nrow(diamonds)) %% 2 == 1)

