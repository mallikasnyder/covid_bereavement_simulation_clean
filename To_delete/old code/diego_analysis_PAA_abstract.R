#Set working directory
# setwd("~/covid_simulation/")

#Load packages
library(tidyverse)
require(kableExtra)
# require(stargazer)

#Load data
# data <- get(load(file = "../Output/presentation_data.RData"))
data<- get(load(file = "../Data/final_data_paa.RData"))

# Burden of excess bereavement
# Create a table for PAA abstract
# Defined as:
# \begin{equation}
# ebr(r,a) = \frac{b(r,a|factual) - b(r,a|counterfactual)}{b(r,a|counterfactual)}
# \end{equation}


# 1. Data format --------------

# age_br <- c(0, 14, 44, Inf)
# age_labs <- c("0-14", "15-44", "45+")

kin_allowed <- c("nuclear", "extended", "parents", "gparents", "unclesaunts")
age_allowed <- c("0-14", "15-29", "30-44", "45-64")
# country_allowed <- sort(c("United_States_of_America", "Sweden"))
country_allowed <- sort(c("United_Kingdom", "Spain"))

# Get bereavemetn during first half of 2020 for all simulations
data2 <- 
  data$kin_bb %>%
  # TEMPROARY!
  filter(!is.na(scenario)) %>% 
  mutate(
    sex = ifelse(grepl("f1", category), "F", "M")
    , age = gsub("f[0-1]{1}", "", category)
  ) %>% 
  # select(country, sim.id, scenario, age, sex, kintype, mean_pre, mean_post) %>% 
  # Reduce simulations to one obs per country
  # group_by(country, scenario, age, sex, kintype) %>%
  group_by(country, scenario, age, kintype) %>%
  summarise(
    mean_pre = mean(mean_pre, na.rm = T)
    , mean_post = mean(mean_post, na.rm = T)
  ) %>% 
  ungroup() 
  

# 2. Get excess bereavement rate ============
# Positive values means that more kin loss in covid scenario
ebr <-
  data2 %>% 
  select(scenario, kintype, country, age, value = mean_post) %>%
  # select(scenario, kintype, country, age, sex, value = mean_post) %>% 
  pivot_wider(
    names_from = scenario
    , values_from = value
  ) %>%  
  mutate(
    # value = round( (covid-other)/other *100)
    value = (other-covid)/covid *100
    , value = ifelse(is.nan(value)|is.infinite(value), 0, value)
    , value = round(value, 1)
  ) %>% 
  select(kintype, age, country,  value)
  # select(kintype, age, country,  sex, value)


# 3. Create df for exporting ---------
ebr_small <- 
  ebr %>% 
  filter(country %in% country_allowed) %>%
  filter(kintype %in% kin_allowed) %>% 
  filter(age %in% age_allowed) %>%
  pivot_wider(names_from = kintype, values_from = value) %>% 
  arrange(country, age) %>% 
  # select(country, sex, age, parents, uncles = unclesaunts, grandparents = gparents, nuclear, extended)
  select(country, age, parents, uncles = unclesaunts, grandparents = gparents, nuclear, extended)

# Add average

mean_df <-
  ebr_small %>% 
  pivot_longer(-c(country, age), names_to = "variable") %>% 
  group_by(country, variable) %>% 
  summarise(value = round(mean(value),1)) %>% 
  ungroup %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate(age = "0_All Ages")

prelim_est <- 
  bind_rows(ebr_small, mean_df) %>% 
  arrange(country, age) %>% 
  mutate(age = gsub("0_", "", age)) %>% 
  # ebr_small %>% 
  # select(-sex) %>%
  select(-country) %>% 
  kable(
    "latex"
    , caption = "Excess Bereavement Rate by age for an ego surviving the Covid-19 pandemic (percent)", label = "ebr",
    booktabs = T, escape = T, 
    col.names = c("Age Interval", "Parents", "Aunts/Uncles", "Grandparents", "Nuclear", "Extended")
  ) %>% 
  kable_styling() %>%
  pack_rows(
    index = c(
      # "United States" = length(age_allowed) + 1
      # , "Sweden" = length(age_allowed) + 1
      "United Kingdom" = length(age_allowed) + 1
      , "Spain" = length(age_allowed) + 1
    )
  ) %>% 
  footnote(
    general = c("Positive values indicate that a simulated individual in the COVID-19 simulations experienced more bereavement compared to an average simulated individual in the Counterfactual simulation in the first half of 2020."),
           threeparttable = T
    )

write(
  prelim_est
  , file = "../Output/paa_excess_bereavement_rate.tex")

