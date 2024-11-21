library(tidyverse)
library(lmtest)
library(sandwich)
library(Metrics)
library(car)
data <- read_csv("smoking.csv")



# ----- Task 3 ----- 

data %>%
  mutate(smkban = as.character(smkban)) %>%
  bind_rows(., data %>% mutate(smkban = as.character("all"))) %>%
  group_by(smkban) %>%
  summarise(probability_of_smoking = mean(smoker))

# A smoke ban reduces the probability that a worker is a smoker by 7.8 percentage points

lm.3.1 <- lm(smoker ~ smkban, data)
coeftest(lm.3.1, vcov.=vcovHC(lm.3.1, "HC1"))

# The t value is -8.7 for a null hypothesis that there is no difference in the probability of smoking between workers affected by a smoke ban and workers not affected by such a ban
# Thus, reject the null hypothesis at 5% significance

lm.3.2 <- lm(smoker ~ smkban + female + age + age^2 + hsdrop + hsgrad + colsome + colgrad + black + hispanic, data)
coeftest(lm.3.2, vcov.=vcovHC(lm.3.2, "HC1"))

# In lm.3.2, a smoke ban reduces the probability that a worker is a smoker by 4.5 percentage points, other regressors held constant
# Compared to lm.3.1, the effect of a smoke ban is weaker in lm.3.2

# A smoke ban is more likely to be implemented in a workplace with higher prestige. In such a workplace people will have a higher level of education on average.
# This means that values of smoke ban and education dummies will be correlated. A reasonable assumption is that people with higher education are less likely to smoke, 
# which is confirmed by the decreasing positive coefficients on hsdrop, hsgrad, colsome and colgrad in lm.3.2. This means that level of education is a determinant of whether a worker smokes.
# In lm.3.1, coefficient on smkban is subject to omitted variable bias from education level and is therefore lower than in reality.

coeftest(lm.3.2, vcov.=vcovHC(lm.3.2, "HC1"))[2,]
# The t value is -5.1 for a null hypothesis that there is no difference in the probability of smoking between workers affected by a smoke ban and workers not affected by such a ban for lm.3.2
# Thus, reject the null hypothesis at 5% significance

myH0 <- c("hsdrop", "hsgrad", "colsome", "colgrad")
linearHypothesis(lm.3.2, myH0, vcov. = vcovHC(lm.3.2, "HC1"))
# The null hypothesis that coefficients on education dummies are all zero is rejected at 5% significance.
# Holding other regressors constant, high school dropouts are 31 percentage points more likely to smoke than master's degree or higher
#                                    high school graduates are 22.4 percentage points more likely to smoke than master's degree or higher
#                                    those who went to college but did not graduate are 15.6 percentage points more likely to smoke than master's degree or higher
#                                    college graduates are 4.2 percentage points more likely to smoke than master's degree or higher



# ----- Task 4 ----- 

probit.4.1 <- glm(smoker ~ smkban + female + age + age^2 + hsdrop + hsgrad + colsome + colgrad + black + hispanic, data = data, family = binomial(link = probit))
summary(probit.4.1)
