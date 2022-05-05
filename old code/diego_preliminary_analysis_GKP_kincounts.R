#Set working directory
# setwd("~/covid_simulation/")

#Load packages
library(tidyverse)
require(kableExtra)
# require(stargazer)

#Load data
data <- get(load(file = "Data/abstract_data.RData"))

# 1. Data format --------------

age_br <- c(0, 14, 44, Inf)
age_labs <- c("0-14", "15-44", "45+")

#Tidy/clean data (this version is used in tables)
data2 <- data %>%
  mutate(sex = if_else(sex == "1", "Female", "Male"),
         cohort = as.numeric(cohort),
         age.2020 = 2020-cohort,
         age.int = if_else(age.2020 < 15, "0-14", if_else(between(age.2020, 15, 64), "15-64",
                    if_else(between(age.2020, 65, 74), "65-74", if_else(between(age.2020, 75, 84), "75-84",
                    if_else(age.2020 > 84, "85+", NA_character_))))),
         age.int = ordered(as.factor(age.int), levels = c("0-14", "15-64", "65-74", "75-84", "85+")),
         age_gr = cut(age.2020, age_br, age_labs, include.lowest = T),
         scenario = if_else(scenario == "covid", "COVID-19", "Counterfactual"),
         scenario = ordered(as.factor(scenario), levels = c("COVID-19", "Counterfactual")),
         country = if_else(country == "UnitedStates", "United States", country),
         kin.type = if_else(kin.type == "gparents", "Grandparents", 
                    if_else(kin.type == "gunclesaunts", "Grand-Aunts/Uncles", 
                    if_else(kin.type == "parents", "Parents",
                    if_else(kin.type == "unclesaunts", "Aunts/Uncles", 
                    if_else(kin.type == "siblings", "Siblings", 
                    if_else(kin.type == "firstcousins", "First Cousins", 
                    if_else(kin.type == "children", "Children",
                    if_else(kin.type == "spouse", "Spouse",
                    if_else(kin.type == "parentsofspouse", "Parents-in-Law", kin.type))))))))),
         kin.type = ordered(as.factor(kin.type), 
                            levels = c("Grandparents", "Grand-Aunts/Uncles", 
                                       "Parents", "Aunts/Uncles", 
                                       "Siblings", "First Cousins", 
                                       "Spouse", "Parents-in-Law",
                                       "Children"))) %>%
  group_by(country, scenario, age.2020, sex, kin.type) %>%
  pivot_wider(names_from = time, values_from = num_kin) %>%
  mutate(
    difference = postcovid - precovid
    , difference_relative = 1 - (postcovid / precovid)
    , difference_relative = ifelse(is.nan(difference_relative), 0, difference_relative)
    )

# 2. Results ------------

#Data for graph

# 2.1. Pre-post-covid barplot ==========

data2 %>% 
  filter(kin.type %in% c("Grandparents", "Grand-Aunts/Uncles", "Parents", "Aunts/Uncles", "First Cousins")) %>%
  # group_by(country, kin.type, scenario, sex, age_gr) %>% 
  group_by(country, kin.type, scenario, age_gr) %>% 
  summarize(
    # value = mean(difference, na.rm = T)
    value = mean(difference_relative, na.rm = T)
    ) %>% 
  ungroup() %>%
  ggplot() + 
  geom_col(
    aes(x = age_gr, y = value, fill = scenario)
    , position = "dodge"
    ) +
  facet_grid(kin.type ~ country) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3), labels = function(br) round(br*100,1)) +
  labs(x = "Age of an average person in 2020", y = "Reduction in kin alive Pre and Post Covid (%)",
       color = "Country", fill = "Scenario") +
  theme(
    legend.position = "bottom"
  )

# ggsave(file = "~/covid_simulation/Output/prelim_output_selected_countries2.pdf")
ggsave(file = "Output/prelim_output_selected_countries2.pdf", width = 13, height = 15, units = "cm")

# 2.2. Kin loss: relative change ==========

pre_post <- 
  data2 %>% 
  # filter(kin.type %in% c("Grandparents", "Grand-Aunts/Uncles", "Parents", "Aunts/Uncles", "First Cousins")) %>%
  # group_by(country, kin.type, scenario, sex, age_gr) %>% 
  group_by(country, kin.type, scenario, sex, age_gr) %>% 
  summarize(
    value = mean(difference_relative, na.rm = T)
    , value = round(value*100, 2)
  ) %>% 
  ungroup() 


pre_post_wide <- 
  pre_post %>%
  pivot_wider(
    id_cols = c("country", "kin.type")
    , names_from = c("scenario", "sex", "age_gr")
    , values_from = value
  )

pre_post_wide <- pre_post_wide[ , c(1:2, order(colnames(pre_post_wide[,-c(1:2)]))+2) ]

pre_post_out <-
  pre_post_wide %>% 
  arrange(country, kin.type) %>%
  select(-country) %>%
  # Make bold excess bereavement [PEND]
  kable(
    "latex"
    , caption = "Reduction in kin alive Pre and Post Covid (percent change)", label = "prelim_pre_post",
    booktabs = T, escape = T, 
    col.names = c("Age Interval", "0-14", "15-44", "45+", "0-14", "15-44", "45+","0-14", "15-44", "45+", "0-14", "15-44", "45+")
  ) %>% 
  kable_styling() %>%
  pack_rows(
    index = c(
      "Germany" = length(unique(data_est$kin.type))
      , "Sweden" = length(unique(data_est$kin.type))
      , "USA" = length(unique(data_est$kin.type))
    )
  ) %>%
  add_header_above(c(" " = 1, "Female" = 3, "Male" = 3, "Female" = 3, "Male" = 3)) %>% 
  add_header_above(c(" " = 1, "COVID-19 simulations" = 6, 
                     "Counterfactual simulations" = 6), underline = F) %>% 
  footnote(
    general = c("This table calculates the average change in the number of relatives alive 
      for an individual of a particular sex and age interval surviving past the end of the 
      COVID-19-adjusted mortality period or the equivalent month in the counterfactual period 
      for a given country. 
      The percent change is estimates as 1 - (postcovid / precovid).
      These calculations are derived from 5 SOCSIM simulations for each 
      scenario for Germany, Sweden, and the United States.
      * $\\leq$ 0.01"
    ), threeparttable = T)


write(
  pre_post_out
  # gsub("\\textbackslash{}","\\", prelim_est)
  , file = "Output/prelim_output_selected_countries2.tex")


# !2.3. Kin loss: absolute change ==========

pre_post <- 
  data2 %>% 
  # filter(kin.type %in% c("Grandparents", "Grand-Aunts/Uncles", "Parents", "Aunts/Uncles", "First Cousins")) %>%
  # group_by(country, kin.type, scenario, sex, age_gr) %>% 
  group_by(country, kin.type, scenario, sex, age_gr) %>% 
  summarize(
    value = mean(difference, na.rm = T)
    , value = round(abs(value), 4)
  ) %>% 
  ungroup() 

# Difference between number of relatives lost in 
# each scenario

pre_post_diff <-
  pre_post %>% 
  pivot_wider(
    names_from = scenario
    , values_from = value
  ) %>% 
  mutate(
    value = round(`COVID-19` - Counterfactual, 2)
    # Make bold excess bereavement
    , value2 = ifelse(value < 0, cell_spec(value, format = "latex", color = "red"), value)
    , value = ifelse(value > 0, cell_spec(value, format = "latex", color = "blue"), value2)
    , scenario = "covid-counter"
  ) %>% 
  select(scenario, kin.type, age_gr, country, sex, value)

pre_post_wide <- 
  bind_rows(
    pre_post %>% mutate(value = as.character(value), scenario = as.character(scenario))
    , pre_post_diff
    ) %>% 
  filter(scenario %in% c("covid-counter")) %>% 
  mutate(value = ifelse(value == 0, "*", value)) %>% 
  pivot_wider(
    id_cols = c("country", "kin.type")
    , names_from = c("scenario", "sex", "age_gr")
    , values_from = value
  )

pre_post_wide <- pre_post_wide[ , c(1:2, order(colnames(pre_post_wide[,-c(1:2)]))+2) ]

pre_post_out <-
  pre_post_wide %>% 
    arrange(country, kin.type) %>%
    select(-country) %>%
    # Make bold excess bereavement [PEND]
    kable(
      "latex"
      , caption = "Kin loss during first half of 2020: Absolute difference between COVID-19 and Counterfactual simulations", label = "prelim_pre_post",
      booktabs = T, escape = F, 
      col.names = c("Age Interval", "0-14", "15-44", "45+", "0-14", "15-44", "45+")
    ) %>% 
    kable_styling() %>%
    pack_rows(
      index = c(
        "Germany" = length(unique(data_est$kin.type))
        , "Sweden" = length(unique(data_est$kin.type))
        , "USA" = length(unique(data_est$kin.type))
      )
    ) %>%
    add_header_above(c(" " = 1, "Female" = 3, "Male" = 3)) %>% 
    # add_header_above(c(" " = 1, "COVID-19 simulations" = 3, 
    #                    "Counterfactual simulations" = 6), underline = F) %>% 
  footnote(
    general = c("Mean number of kin death events experienced by an individual of a particular 
    sex and age during first half of 2020. 
    The values represents the mean difference between the number of kin deaths in the COVID-19 
    and Counterfactual simulations. 
    Higher values indicate that simulated individuals in the COVID-19 simulations
    lost, on average, more relatives than simulated individuals in the counterfactual simulations.
    These calculations are derived from 5 SOCSIM simulations for each 
    scenario for Germany, Sweden, and the United States. * $\\leq$ 0.01"
      ), threeparttable = T)

  
  write(
    pre_post_out
    # gsub("\\textbackslash{}","\\", prelim_est)
    , file = "Output/prelim_output_selected_countries_diff.tex")
    

# 3. Kin count Tables ------------------

# 3.1 By sex ===============

#Making a summary table
#We want kin counts post-covid
# data_est <-
data3 <- 
  data2 %>%
  ungroup() %>%
  mutate(short.age.int = if_else(age.2020 < 15, "0-14", 
                                 if_else(between(age.2020, 15, 44), "15-44", 
                                         "45+"))) %>%
  select(-c(age.int))
  
data_sum <-
  data3 %>% 
  group_by(scenario, kin.type, short.age.int, sex) %>% 
  summarize(
    kin_count = round(mean(postcovid, na.rm = T), 2)
    , kin_count_sd = round(sd(postcovid, na.rm = T), 2)
    # , difference = 
  ) %>%
  ungroup() %>%
  mutate(
    value = paste0(kin_count, "(", kin_count_sd, ")")
    # , kin_count= NULL
    # , kin_count_sd = NULL
  )

# Absolute change
data_diff_abs <-
  data_sum %>%
  pivot_wider(
    -c(value, kin_count_sd)
    , names_from = scenario
    , values_from = kin_count
    ) %>% 
  mutate(
    value = round(`COVID-19` - Counterfactual, 2)
    # Make bold excess bereavement
    , value = ifelse(value < 0, cell_spec(value, format = "latex", color = "blue"), value)
    , value = ifelse(value > 0, cell_spec(value, format = "latex", color = "red"), value)
    , value = as.character(value)
    , scenario = "covid-count"
  ) %>% 
  select(scenario, kin.type, short.age.int, sex, value)
    
# Relative change
data_diff_rel <-
  data_sum %>%
  pivot_wider(
    -c(value, kin_count_sd)
    , names_from = scenario
    , values_from = kin_count
  ) %>% 
  mutate(
    # value = round((Counterfactual/`COVID-19`-1)*100)
    # value = round((1-`COVID-19`/Counterfactual)*100)
    value = round( (`COVID-19`-Counterfactual)/Counterfactual *100)
    , value = ifelse(is.nan(value)|is.infinite(value), 0, value)
    # Make bold excess bereavement
    , value = ifelse(value < 0, cell_spec(value, format = "latex", color = "blue"), value)
    , value = ifelse(value > 0, cell_spec(value, format = "latex", color = "red"), value)
    , value = as.character(value)
    , scenario = "(covid-count)/count"
  ) %>% 
  select(scenario, kin.type, short.age.int, sex, value)

data_est <-
  bind_rows(
    data_sum %>% 
      mutate(scenario = as.character(scenario)) %>% 
      select(-starts_with("kin_count"))
    , data_diff_abs
    , data_diff_rel
    ) %>% 
    pivot_wider(
      id_cols = c("sex", "kin.type")
      , names_from = c("scenario", "short.age.int")
      , values_from = value
    )
    

prelim_est <- 
  data_est %>%
  arrange(sex, kin.type) %>%
  select(-sex, -starts_with("Counter"), - starts_with("COVID-19")) %>% 
  # Make bold excess bereavement [PEND]
  kable("latex", caption = "Difference in the expected number of living kin between the 'COVID-19' and 'Counterfactual' simulations", label = "prelim_est",
      booktabs = T, escape = F,
      col.names = c("Age Interval", "0-14", "15-44", "45+", "0-14", "15-44", "45+")) %>%
  kable_styling() %>%
  pack_rows(index = c("Female" = length(unique(data_est$kin.type)), 
                      "Male" = length(unique(data_est$kin.type)))) %>%
  # add_header_above(c(" " = 1, "Covid-Counterfactual" = 3, 
  #                    "(Covid-Counterfactual)/Counterfactual*100" = 3)) %>%
  add_header_above(c(" " = 1, "Absolute difference" = 3, 
                     "Relative difference %" = 3), underline = F) %>%
  footnote(general = c("Absolute difference: Covid-Counterfactual. Relative difference: (Covid-Counterfactual)/Counterfactual*100"),
           threeparttable = T)
  

  
write(
  prelim_est
  # gsub("\\textbackslash{}","\\", prelim_est)
  , file = "Output/prelim_estimates_diff.tex")

# 3.2. By country ===============

#Making a summary table
#We want kin counts post-covid
# data_est <-
data3 <- 
  data2 %>%
  ungroup() %>%
  mutate(short.age.int = if_else(age.2020 < 15, "0-14", 
                                 if_else(between(age.2020, 15, 44), "15-44", 
                                         "45+"))) %>%
  select(-c(age.int))

data_sum <-
  data3 %>% 
  group_by(scenario, kin.type, short.age.int, country) %>% 
  summarize(
    kin_count = round(mean(postcovid, na.rm = T), 2)
    , kin_count_sd = round(sd(postcovid, na.rm = T), 2)
    # , difference = 
  ) %>%
  ungroup() %>%
  mutate(
    value = paste0(kin_count, "(", kin_count_sd, ")")
    # , kin_count= NULL
    # , kin_count_sd = NULL
  )

# Absolute change
data_diff_abs <-
  data_sum %>%
  pivot_wider(
    -c(value, kin_count_sd)
    , names_from = scenario
    , values_from = kin_count
  ) %>% 
  mutate(
    value = round(`COVID-19` - Counterfactual, 2)
    # Make bold excess bereavement
    , value = ifelse(value < 0, cell_spec(value, format = "latex", color = "blue"), value)
    , value = ifelse(value > 0, cell_spec(value, format = "latex", color = "red"), value)
    , value = as.character(value)
    , scenario = "covid-count"
  ) %>% 
  select(scenario, kin.type, short.age.int, country, value)

# Relative change
data_diff_rel <-
  data_sum %>%
  pivot_wider(
    -c(value, kin_count_sd)
    , names_from = scenario
    , values_from = kin_count
  ) %>% 
  mutate(
    # value = round((Counterfactual/`COVID-19`-1)*100)
    # value = round((1-`COVID-19`/Counterfactual)*100)
    value = round( (`COVID-19`-Counterfactual)/Counterfactual *100)
    , value = ifelse(is.nan(value)|is.infinite(value), 0, value)
    # Make bold excess bereavement
    , value = ifelse(value < 0, cell_spec(value, format = "latex", color = "blue"), value)
    , value = ifelse(value > 0, cell_spec(value, format = "latex", color = "red"), value)
    , value = as.character(value)
    , scenario = "(covid-count)/count"
  ) %>% 
  select(scenario, kin.type, short.age.int, country, value)

data_est <-
  bind_rows(
    data_sum %>% 
      mutate(scenario = as.character(scenario)) %>% 
      select(-starts_with("kin_count"))
    , data_diff_abs
    , data_diff_rel
  ) %>% 
  pivot_wider(
    id_cols = c("country", "kin.type")
    , names_from = c("scenario", "short.age.int")
    , values_from = value
  )


prelim_est <- 
  data_est %>%
  arrange(country, kin.type) %>%
  select(-country, -starts_with("Counter"), - starts_with("COVID-19")) %>% 
  # Make bold excess bereavement [PEND]
  kable("latex", caption = "Difference in the expected number of living kin between the 'COVID-19' and 'Counterfactual' simulations", label = "prelim_est",
        booktabs = T, escape = F, 
        col.names = c("Age Interval", "0-14", "15-44", "45+", "0-14", "15-44", "45+")) %>%
  kable_styling() %>%
  pack_rows(
    index = c(
      "Germany" = length(unique(data_est$kin.type))
      , "Sweden" = length(unique(data_est$kin.type))
    , "USA" = length(unique(data_est$kin.type))
    )
    ) %>%
  # add_header_above(c(" " = 1, "Covid-Counterfactual" = 3, 
  #                    "(Covid-Counterfactual)/Counterfactual*100" = 3)) %>%
  add_header_above(c(" " = 1, "Absolute difference" = 3, 
                     "Relative difference %" = 3), underline = F) %>%
  footnote(general = c("Absolute difference: Covid-Counterfactual. Relative difference: (Covid-Counterfactual)/Counterfactual*100"),
           threeparttable = T)



write(
  prelim_est
  # gsub("\\textbackslash{}","\\", prelim_est)
  , file = "Output/prelim_estimates_country_diff.tex")

# 3.2. By country and sex ===============

#Making a summary table
#We want kin counts post-covid
# data_est <-
data3 <- 
  data2 %>%
  ungroup() %>%
  mutate(short.age.int = if_else(age.2020 < 15, "0-14", 
                                 if_else(between(age.2020, 15, 44), "15-44", 
                                         "45+"))) %>%
  select(-c(age.int))

data_sum <-
  data3 %>% 
  group_by(scenario, kin.type, short.age.int, country, sex) %>% 
  summarize(
    kin_count = round(mean(postcovid, na.rm = T), 2)
    , kin_count_sd = round(sd(postcovid, na.rm = T), 2)
    # , difference = 
  ) %>%
  ungroup() %>%
  mutate(
    value = paste0(kin_count, "(", kin_count_sd, ")")
    # , kin_count= NULL
    # , kin_count_sd = NULL
  )

# Absolute change
data_diff_abs <-
  data_sum %>%
  pivot_wider(
    -c(value, kin_count_sd)
    , names_from = scenario
    , values_from = kin_count
  ) %>% 
  mutate(
    value = round(`COVID-19` - Counterfactual, 2)
    # Make bold excess bereavement
    , value = ifelse(value < 0, cell_spec(value, format = "latex", color = "blue"), value)
    , value = ifelse(value > 0, cell_spec(value, format = "latex", color = "red"), value)
    , value = as.character(value)
    , scenario = "a_covid-count"
  ) %>% 
  select(scenario, kin.type, short.age.int, country, sex, value)

# Relative change
data_diff_rel <-
  data_sum %>%
  pivot_wider(
    -c(value, kin_count_sd)
    , names_from = scenario
    , values_from = kin_count
  ) %>% 
  mutate(
    # value = round((Counterfactual/`COVID-19`-1)*100)
    # value = round((1-`COVID-19`/Counterfactual)*100)
    value = round( (`COVID-19`-Counterfactual)/Counterfactual *100)
    , value = ifelse(is.nan(value)|is.infinite(value), 0, value)
    # Make bold excess bereavement
    , value = ifelse(value < 0, cell_spec(value, format = "latex", color = "blue"), value)
    , value = ifelse(value > 0, cell_spec(value, format = "latex", color = "red"), value)
    , value = as.character(value)
    , scenario = "b_(covid-count)/count"
  ) %>% 
  select(scenario, kin.type, short.age.int, country,  sex, value)

data_est <-
  bind_rows(
    data_sum %>% 
      mutate(scenario = as.character(scenario)) %>% 
      select(-starts_with("kin_count"))
    , data_diff_abs
    , data_diff_rel
  ) %>% 
  pivot_wider(
    id_cols = c("country", "kin.type")
    , names_from = c("scenario", "sex", "short.age.int")
    , values_from = value
  )

# data_bind <-
#   bind_rows(
#     data_sum %>% 
#       mutate(scenario = as.character(scenario)) %>% 
#       select(-starts_with("kin_count"))
#     , data_diff_abs
#     , data_diff_rel
#   ) 
# 
# data_male <- 
#   data_bind %>%
#   filter(sex == "Male") %>% 
#   select(-sex) %>% 
#   mutate(scenario = paste0(scenario, "_male")) %>% 
#   pivot_wider(
#     id_cols = c("country", "kin.type")
#     , names_from = c("scenario", "short.age.int")
#     , values_from = value
#   ) %>% 
#   select(-starts_with("Counter"), - starts_with("COVID-19"))
# 
# data_female <- 
#   data_bind %>%
#   filter(sex == "Female") %>% 
#   select(-sex) %>% 
#   mutate(scenario = paste0(scenario, "_female")) %>% 
#   pivot_wider(
#     id_cols = c("country", "kin.type")
#     , names_from = c("scenario", "short.age.int")
#     , values_from = value
#   ) %>% 
#   select(-starts_with("Counter"), - starts_with("COVID-19"))


# Re-order olumns
data_est <- data_est[ , c(1:2, order(colnames(data_est[,-c(1:2)]))+2) ]

prelim_est <- 
  data_est %>%
  arrange(country, kin.type) %>%
  select(-country,-starts_with("Counter"), - starts_with("COVID-19")) %>% 
  # Make bold excess bereavement [PEND]
  kable(
    "latex"
    , caption = "Difference in the expected number of living kin between the 'COVID-19' and 'Counterfactual' simulations", label = "prelim_est",
        booktabs = T, escape = F, 
        col.names = c("Age Interval", "0-14", "15-44", "45+", "0-14", "15-44", "45+","0-14", "15-44", "45+", "0-14", "15-44", "45+")
        ) %>% 
  kable_styling() %>%
  pack_rows(
    index = c(
      "Germany" = length(unique(data_est$kin.type))
      , "Sweden" = length(unique(data_est$kin.type))
      , "USA" = length(unique(data_est$kin.type))
    )
  ) %>%
  add_header_above(c(" " = 1, "Female" = 3, "Male" = 3, "Female" = 3, "Male" = 3)) %>% 
  add_header_above(c(" " = 1, "Absolute difference" = 6, 
                     "Relative difference %" = 6), underline = F) %>%
  footnote(general = c("Absolute difference: Covid-Counterfactual. Relative difference: (Covid-Counterfactual)/Counterfactual*100"),
           threeparttable = T)



write(
  prelim_est
  # gsub("\\textbackslash{}","\\", prelim_est)
  , file = "Output/prelim_estimates_country_sex_diff.tex")

# !3.3. Relative diff by country and sex ===============


#Making a summary table
#We want kin counts post-covid
# data_est <-
data3 <- 
  data2 %>%
  ungroup() %>%
  mutate(short.age.int = if_else(age.2020 < 15, "0-14", 
                                 if_else(between(age.2020, 15, 44), "15-44", 
                                         "45+"))) %>%
  select(-c(age.int))

data_sum <-
  data3 %>% 
  group_by(scenario, kin.type, short.age.int, country, sex) %>% 
  summarize(
    kin_count = round(mean(postcovid, na.rm = T), 2)
    , kin_count_sd = round(sd(postcovid, na.rm = T), 2)
    # , difference = 
  ) %>%
  ungroup() %>%
  mutate(
    value = paste0(kin_count, "(", kin_count_sd, ")")
    # , kin_count= NULL
    # , kin_count_sd = NULL
  )

# Absolute change
data_diff_abs <-
  data_sum %>%
  pivot_wider(
    -c(value, kin_count_sd)
    , names_from = scenario
    , values_from = kin_count
  ) %>% 
  mutate(
    value = round(`COVID-19` - Counterfactual, 2)
    # Make bold excess bereavement
    , value = ifelse(value < 0, cell_spec(value, format = "latex", color = "blue"), value)
    , value = ifelse(value > 0, cell_spec(value, format = "latex", color = "red"), value)
    , value = as.character(value)
    , scenario = "a_covid-count"
  ) %>% 
  select(scenario, kin.type, short.age.int, country, sex, value)

# Relative change
data_diff_rel <-
  data_sum %>%
  pivot_wider(
    -c(value, kin_count_sd)
    , names_from = scenario
    , values_from = kin_count
  ) %>% 
  mutate(
    # value = round((Counterfactual/`COVID-19`-1)*100)
    # value = round((1-`COVID-19`/Counterfactual)*100)
    value = round( (`COVID-19`-Counterfactual)/Counterfactual *100)
    , value = ifelse(is.nan(value)|is.infinite(value), 0, value)
    # Make bold excess bereavement
    , value = ifelse(value < 0, cell_spec(value, format = "latex", color = "blue"), value)
    , value = ifelse(value > 0, cell_spec(value, format = "latex", color = "red"), value)
    , value = as.character(value)
    , scenario = "b_(covid-count)/count"
  ) %>% 
  select(scenario, kin.type, short.age.int, country,  sex, value)

data_est <-
  bind_rows(
    data_sum %>% 
      mutate(scenario = as.character(scenario)) %>% 
      select(-starts_with("kin_count"))
    , data_diff_abs
    , data_diff_rel
  ) %>% 
  mutate(value = ifelse(value == 0, "*", value)) %>% 
  pivot_wider(
    id_cols = c("country", "kin.type")
    , names_from = c("scenario", "sex", "short.age.int")
    , values_from = value
  )
# Re-order olumns
data_est <- data_est[ , c(1:2, order(colnames(data_est[,-c(1:2)]))+2) ]

prelim_est <- 
  data_est %>%
  arrange(country, kin.type) %>%
  select(-country,-starts_with("Counter"), - starts_with("COVID-19"), - starts_with("a_covid")) %>% 
  # Make bold excess bereavement [PEND]
  kable(
    "latex"
    , caption = "Relative difference in the expected number of living kin between the 'COVID-19' and 'Counterfactual' simulations in July 2020", label = "prelim_est",
    booktabs = T, escape = F, 
    col.names = c("Age Interval", "0-14", "15-44", "45+", "0-14", "15-44", "45+")
  ) %>% 
  kable_styling() %>%
  pack_rows(
    index = c(
      "Germany" = length(unique(data_est$kin.type))
      , "Sweden" = length(unique(data_est$kin.type))
      , "USA" = length(unique(data_est$kin.type))
    )
  ) %>%
  add_header_above(c(" " = 1, "Female" = 3, "Male" = 3)) %>% 
  footnote(general = c("Relative difference: (Covid-Counterfactual)/Counterfactual*100. Negative numbers indicate that a simulated individual in the COVID-19 simulations has fewer relatives alive compared to an average simulated individual in the Counterfactual simulation. * $\\leq$ 0.5"),
           threeparttable = T)



write(
  prelim_est
  # gsub("\\textbackslash{}","\\", prelim_est)
  , file = "Output/prelim_estimates_country_sex_diff_small.tex")


# 4. Bereavement multiplier ==============

