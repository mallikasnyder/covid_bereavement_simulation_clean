#Set working directory
setwd("~/covid_simulation/")

#Load packages
require(kableExtra)
require(stargazer)

#Load data
data <- get(load(file = "Data/abstract_data.RData"))

#Tidy/clean data (this version is used in tables)
data2 <- data %>%
  mutate(sex = if_else(sex == "1", "Female", "Male"),
         cohort = as.numeric(cohort),
         age.2020 = 2020-cohort,
         age.int = if_else(age.2020 < 15, "0-14", if_else(between(age.2020, 15, 64), "15-64",
                    if_else(between(age.2020, 65, 74), "65-74", if_else(between(age.2020, 75, 84), "75-84",
                    if_else(age.2020 > 84, "85+", NA_character_))))),
         age.int = ordered(as.factor(age.int), levels = c("0-14", "15-64", "65-74", "75-84", "85+")),
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
  mutate(difference = postcovid - precovid)

#Data for graph

#Making a first graph 
data2 %>%
  summarize(mean.diff = mean(difference, na.rm = T)) %>%
  filter(kin.type %in% c("Grandparents", "Grand-Aunts/Uncles", "Parents", "Aunts/Uncles")) %>%
  ungroup() %>%
  ggplot() + geom_line(aes(x = age.2020, y = mean.diff, 
                           color = country, linetype = scenario)) +
  facet_grid(rows = vars(kin.type), cols = vars(sex)) +
  labs(x = "Age in 2020", y = "Mean Diff. between Kin Counts Pre and Post Covid",
       color = "Country", linetype = "Scenario")

ggsave(file = "~/covid_simulation/Output/prelim_output_selected_countries.pdf")

#Making a summary table
#We want kin counts post-covid
data_est <- data2 %>%
  ungroup() %>%
  mutate(short.age.int = if_else(age.2020 < 15, "0-14", 
                                 if_else(between(age.2020, 15, 44), "15-44", 
                                         "45+"))) %>%
  select(-c(age.int)) %>%
  group_by(scenario, kin.type, short.age.int, sex) %>%
  summarize(kin_count = round(mean(postcovid, na.rm = T), 2),
            kin_count_sd = round(sd(postcovid, na.rm = T), 2)) %>%
  mutate(value = paste0(kin_count, "(", kin_count_sd, ")"),
         kin_count= NULL, kin_count_sd = NULL) %>%
  pivot_wider(id_cols = c("sex", "kin.type"), names_from = c("scenario", "short.age.int"), values_from = value)
  
prelim_est <- data_est %>%
  arrange(sex, kin.type) %>%
  select(-c("sex")) %>%
  kbl("latex", caption = "Post-COVID-19 and Post-Counterfactual Kin Counts", label = "prelim_est",
      booktabs = T,
      col.names = c("Age Interval", "0-14", "15-45", "45+", "0-14", "15-45", "45+")) %>%
  kable_styling() %>%
  pack_rows(index = c("Female" = length(unique(data_est$kin.type)), 
                      "Male" = length(unique(data_est$kin.type)))) %>%
  add_header_above(c(" " = 1, "COVID-19" = 3, 
                     "Counterfactual" = 3)) %>%
  footnote(general = c("This table calculates average number of kin, with standard deviations in parentheses, for an individual of a particular sex and age interval surviving past the end of the COVID-19-adjusted mortality period or the equivalent month in the counterfactual period for a given country. These calculations are derived from 5 SOCSIM simulations for each scenario for Germany, Sweden, and the United States."),
           threeparttable = T)
  
save_kable(prelim_est, file = "~/covid_simulation/Output/prelim_estimates.tex")

  
#Making a sample regression table (more thought will need to go into this)
linear.nocontrols <- lm(difference ~ as.character(scenario), data = data2)
linear.country <- lm(difference ~ as.character(scenario) + country, data = data2)
linear.controls <- lm(difference ~ as.character(scenario) + kin.gen + cohort.type + sex , data = data2)
linear.controls <- lm(difference ~ as.character(scenario) + kin.gen + cohort.type + sex , data = data2)
linear.countrycontrols <- lm(difference ~ as.character(scenario) + country + kin.gen + cohort.type + sex , data = data2)

stargazer(linear.nocontrols, linear.country, linear.controls, linear.countrycontrols,
          dep.var.labels = "Diff. in Kin Counts",
          covariate.labels = c("COVID-19", "Sweden", "United States", 
                               "Parents+", "Cohort Pre-1970", "Male"),
          omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.90, single.row=TRUE,
          title = "Preliminary Regression Results", out = "~/covid_simulation/Output/prelim_reg_results.tex")



