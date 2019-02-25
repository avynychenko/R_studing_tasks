exp_data <- read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

library(lme4)

# модель с 2-мя случайными эффектами для переменных subject и scenario

fit_1 <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), exp_data)

# добавили в модель в качестве фиксированной переменной gender

fit_2 <- lmer(frequency ~ attitude + gender + (1|subject) + (1|scenario), exp_data)

# добавляем коэф. наклона для случайных эффектов для социальной ситуации (attitude).

fit_3 <- lmer(frequency ~ attitude + gender + (1 + attitude|subject) + 
               (1 + attitude|scenario), exp_data)