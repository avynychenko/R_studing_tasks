# Нужно провести двухфакторный дисперсионный анализ: влияние факторов doctor, влияние фактора pill и их взаимодействие на temperature. 
#Учесть тот факт, что один и тот же больной принимает разные таблетки, и тот факт, что  один и тот же больной лечится у разных врачей!

df <- read.csv("Pillulkin.csv")
df$patient <- factor(df$patient)
fit2 <- aov(df$temperature ~ doctor*pill + Error(df$patient/(pill*doctor)), df)
summary(fit2)



# Провести однофакторный дисперсионный анализ с повторными измерениями: влияние типа таблетки (pill) на температуру (temperature) с 
#учётом испытуемого (patient). Каково p-value для влияния типа таблеток на температуру?

df <- read.csv("Pillulkin.csv")
df$patient <- factor(df$patient)
fit <- aov(temperature ~ pill + Error(patient/pill), df)
summary(fit)


# Воспользуемся встроенными данными npk, иллюстрирующими влияние применения различных удобрений на урожайность гороха (yield). 
#Нашей задачей будет выяснить, существенно ли одновременное применение азота (фактор N) и фосфата (фактор P). 

npk <- npk

fit <- aov(yield ~ N*P, npk)
summary(fit)





