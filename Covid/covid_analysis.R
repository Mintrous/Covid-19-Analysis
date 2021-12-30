rm(list=ls())
library(Hmisc)

covidData = read.csv("~/R/Covid/COVID19_line_list_data.csv")
describe(covidData) # Hmisc command

# cleaning up death column
covidData$death_dummy = as.integer(covidData$death != 0)

# death rate
sum(covidData$death_dummy)/nrow(covidData)

# Age analysis
# age -> Older people die
dead = subset(covidData, death_dummy == 1)
alive = subset(covidData, death_dummy == 0) # 63 died, 1022 survived

mean(dead$age, na.rm = TRUE) # 68 years old
mean(alive$age, na.rm = TRUE) # 48 years old

# check if that is statistically significant
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)

# if p-value < 0, reject null hypothesis
# the p-value ~ 0, concluding this is statistically significant 


# Gender analysis
man = subset(covidData, gender == "male")
woman = subset(covidData, gender == "female")

mean(man$death_dummy, na.rm = TRUE) # 8.4%
mean(woman$death_dummy, na.rm = TRUE) # 3.7%

# check if that is statistically significant
t.test(man$death_dummy, woman$death_dummy, alternative = "two.sided", conf.level = 0.99)

# 99% confidence: man have from 0.8% to 8.8% higher chance of dying
# p-value ~ 0, concluding this is statistically significant