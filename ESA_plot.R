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
library(sjlabelled)
library(dplyr) 


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





##graphs

df <- data.frame(oct31)



ggplot(df, aes(AdultGCT, RodentPrev, color = Site)) + geom_point() +
  geom_smooth(method = lm, se = FALSE) +
 

ggplot(df, aes(AdultGCT, RodentPrev)) + geom_point() +
  geom_smooth(method = lm, se = FALSE)

##final graphs
ggplot(df, aes(TickDensity, RodentPrev, color = Site)) + geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_colour_discrete(labels = c ("CH3", "HM1", "NC1", "TP1")) +
  xlab("Adult Gulf Coast Tick Density") + ylab("Rodent prevalence") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_blank(), axis.line = element_line(colour = "black"))
                    

ggplot(df, aes(TickDensity, RodentPrev)) + geom_point() +
  geom_smooth(method = lm, se = FALSE)+
  xlab("Adult Gulf Coast Tick Density") + ylab("Rodent prevalence") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


model <<- Tissues %>%
  glm(formula = rp ~ site, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))
plot_model(model, transform= "exp", show.values = T, show.p = T, value.offset = 0.4, 
           axis.title = "Odds Ratio", vline.color = "red", title = "", 
           axis.labels =  c("TP1", "NC1", "HM1")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


model <<- Tissues %>%
  glm(formula = rp ~ tick_density + mammal, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))
plot_model(model, transform = "exp", show.values = F, show.p = T, value.offset = 0.4,
           axis.title = "Odds Ratio", vline.color = "red", title = "", 
           axis.labels =  c("White-footed mouse", "Meadow vole", "House mouse", 
                            "Eastern harvest mouse", "Marsh Rice Rat", "Tick Density")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

model <<- Tissues %>%
  glm(formula = rp ~ mammal, 
      family = "binomial")

summary(model)
exp(cbind(coef(model), confint(model)))
plot_model(model, transform = "exp", show.values = T, show.p = T, value.offset = 0.4,
           axis.title = "Odds Ratio", vline.color = "red", title = "", 
           axis.labels =  c("White-footed mouse", "Meadow vole", "House mouse", 
                            "Eastern harvest mouse", "Marsh Rice Rat")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 


##makes odds ratio line at 0
plot_model(model, transform = NULL, show.values = T, show.p = T, value.offset = 0.4)
