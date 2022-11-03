#test
#hi, Christina


library(tidyverse)



library(readxl)
oct31 <- read_excel("~/Desktop/oct31.xlsx", 
                    sheet = "Sheet3")
View(oct31)
library(readxl)
Tissues <- read_excel("~/Desktop/2020-2021 Non-Ixodes Extracted Rodent Ticks and Tissues.xlsx", 
                                                                       sheet = "Tissue")
View(Tissues)
df <- data.frame(oct31)

library(ggplot2)

ggplot(df, aes(AdultGCT, RodentPrev, color = Site)) + geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(df, aes(AdultGCT, RodentPrev)) + geom_point() +
  geom_smooth(method = lm, se = FALSE)



model <<- Tissues %>%
  glm(formula = Rp ~ Year * Mammal, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))
