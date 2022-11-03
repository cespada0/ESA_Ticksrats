####INITIALIZE####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(readxl)
library(ggplot2)
library(janitor)

####CODE####
oct31 <- read_excel("~/Desktop/oct31.xlsx", 
                    sheet = "Sheet3")
View(oct31)

Tissues <- read_excel("./2020-2021 Non-Ixodes Extracted Rodent Ticks and Tissues.xlsx", 
                                                                       sheet = "Tissue") 

####WRANGLE####
Tissues <- Tissues %>%
  clean_names() %>%
  mutate(log_td = log10(tick_density+1))

View(Tissues)

Rpa_prev <- Tissues %>% 
  group_by(year, site) %>%
  summarise(
    prevalence = mean(rp, na.rm = TRUE),
    n = n(),
    lCI = (prevalence - sqrt((prevalence*(1-prevalence))/n)*1.96),
    uCI = (prevalence + sqrt((prevalence*(1-prevalence))/n)*1.96),
  ) %>%
  filter(n>5)
Rpa_prev 

model <<- Tissues %>%
  glm(formula = rp ~ tick_density + mammal, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))




df <- data.frame(oct31)



ggplot(df, aes(AdultGCT, RodentPrev, color = Site)) + geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(df, aes(AdultGCT, RodentPrev)) + geom_point() +
  geom_smooth(method = lm, se = FALSE)



m