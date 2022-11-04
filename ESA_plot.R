####INITIALIZE####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(readxl)
library(ggplot2)
library(janitor)
install.packages("janitor")

library(knitr)      # beautifying tables
library(car)        # for checking assumptions, e.g. vif etc.
library(broom)      # for tidy model output
library(questionr)  # for odds.ratios
library(sjPlot)     # for plotting results of log.regr.
library(sjmisc)     # for plotting results of log.regr.
library(effects)    # for probability output and plots

install.packages("car", "broom", "questioner", "sjPlot", "sjmisc", "effects")
####CODE####
oct31 <- read_excel("Desktop/oct31.xlsx", 
                    sheet = "Sheet3")
View(oct31)

Tissues <- read_excel("./2020-2021 Non-Ixodes Extracted Rodent Ticks and Tissues.xlsx", 
                                                                       sheet = "Tissue") 

Tissues <- Tissues[which(Tissues$site != "FE"),]
Tissues <- Tissues[which(Tissues$site != "SB"),]


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

## Tick Density and R. pakeri
model <<- Tissues %>%
  glm(formula = rp ~ tick_density,
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))

## Tick Density and mammal species
model <<- Tissues %>%
  glm(formula = rp ~ tick_density + mammal, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))

## mammal species
model <<- Tissues %>%
  glm(formula = rp ~ mammal, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))

## Site 
model <<- Tissues %>%
  glm(formula = rp ~ site, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))


Tissues$total_rodents/Tissues$trap_nights
Rodent_Density <- Tissues$total_rodents/Tissues$trap_nights

## Site and Trap nights
model <<- Tissues %>%
  glm(formula = rp ~ site + Rodent_Density, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))

##trapnights
model <<- Tissues %>%
  glm(formula = rp ~ mammal * site, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))

Tissues$mammal <- as.factor(Tissues$mammal)
Tissues$mammal <- relevel(Tissues$mammal, ref = "Cotton Rat")

model <<- Tissues %>%
  glm(formula = rp ~ tick_density + mammal, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))
plot_model(model, transform = NULL, show.values = T, show.p = T, value.offset = 0.4)

model <<- Tissues %>%
  glm(formula = rp ~ mammal, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))
plot_model(model, transform = NULL, show.values = T, show.p = T, value.offset = 0.4)

model <<- Tissues %>%
  glm(formula = rp ~ site, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))
plot_model(model, transform = NULL, show.values = T, show.p = T, value.offset = 0.4)


##graphs

df <- data.frame(oct31)



ggplot(df, aes(AdultGCT, RodentPrev, color = Site)) + geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(df, aes(AdultGCT, RodentPrev)) + geom_point() +
  geom_smooth(method = lm, se = FALSE)


ggplot(df, aes(TickDensity, RodentPrev, color = Site)) + geom_point() +
  geom_smooth(method = lm, se = FALSE)

ggplot(df, aes(TickDensity, RodentPrev)) + geom_point() +
  geom_smooth(method = lm, se = FALSE)

plot_model(model, transform = NULL, show.values = T, show.p = T, value.offset = 0.4)
