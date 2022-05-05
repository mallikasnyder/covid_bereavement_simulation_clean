#Load functions
#Setting directories and loading functions
setwd("~/covid_simulation/")

#Source functions and packages
source("~/covid_simulation/Kin_death/01_load_functions.R")
source('~/covid_simulation/Kin_death/functions_bereavement.R')

#Require this additional packages
require(broom)

#Load data: currently, this points to the latest version with data for the first wave
data <- get(load(file = "../Data/final_data_firstwave.RData"))

#Find numbers for indices for each country
indices <- data$numbers %>%
  group_by(country) %>%
  summarize(max_index = min(nsims))

#Burden of bereavement object
kin_ebr <-data$kin_bb %>%
  full_join(., indices, by = c("country" = "country")) %>%
  filter(!is.na(scenario)) %>% 
  mutate(sex = ifelse(grepl("f1", category), "F", "M"),
         age = gsub("f[0-1]{1}", "", category),
         category = NULL) %>%
  ungroup() %>%
  group_by(country, scenario, sex, age, kintype) %>%
  mutate(index = row_number(), category = NULL) %>%
  filter(index <= max_index) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("country", "sex", "age", "kintype", "index"), 
              names_from = "scenario", 
              values_from = c("mean_pre_with", "sd_pre_with", "n_withkin", 
                              "mean_post_with", "sd_post_with", "sim.id", "n_losekin", "n_total")) %>%
  mutate(bereavement_covid = (mean_pre_with_covid - mean_post_with_covid),
         bereavement_other = (mean_pre_with_other - mean_post_with_other),
         n_withkin = (n_withkin_covid + n_withkin_other)/2,
         n_losekin = (n_losekin_covid+n_losekin_other)/2,
         n_total = (n_total_covid + n_total_other)/2,
         pc_withkin = (n_withkin/n_total)*100,
         pc_losekin = 100*(n_losekin/n_withkin),
         ebr = 100*(bereavement_covid)/(bereavement_other),
         ccode = countrycode(gsub("_", " ", country), 
                             origin = 'country.name', destination = 'iso3c'))

# Kin ratio object
  kin_structure <- data$kin_ratio %>%
    full_join(., indices, by = c("country" = "country")) %>%
    filter(!is.na(scenario)) %>% 
    mutate(sex = ifelse(grepl("f1", category), "F", "M"),
           age = gsub("f[0-1]{1}", "", category),
           category = NULL) %>%
    ungroup() %>%
    group_by(country, scenario, sex, age, kintype) %>%
    mutate(index = row_number(), category = NULL) %>%
    filter(index <= max_index) %>%
    ungroup() %>%
    pivot_wider(id_cols = c("country", "sex", "age", "kintype", "index"), 
                names_from = "scenario", 
                values_from = c("count_all", "sd_all", "count_female", "count_male",
                                "count_65plus", "count_below65", "n_egos", "sim.id")) %>%
    mutate(ccode = countrycode(gsub("_", " ", country), 
                               origin = 'country.name', destination = 'iso3c'),
           count_all_diff = count_all_covid - count_all_other)

#Death rates object
  death_rates <- data$death_rates %>%
  full_join(., indices, by = c("country" = "country")) %>%
  filter(!is.na(scenario)) %>% 
  mutate(sex = ifelse(fem == 1, "F", "M")) %>%
  ungroup() %>%
  group_by(country, scenario, sex, age) %>%
  mutate(index = row_number(), fem = NULL) %>%
  filter(index <= max_index) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("country", "sex", "age", "index"), 
              names_from = "scenario", 
              values_from = c("n_num", "n_den", "value", "sim.id")) 
  
  em <- death_rates %>%
  filter(age %ni% c("0-14", "all_sum", "all_mid")) %>%
  ungroup() %>%
  group_by(index, country, sim.id_covid, sim.id_other) %>%
  summarize(n_num_covid = sum(n_num_covid, na.rm = T),
            n_num_other = sum(n_num_other, na.rm = T)) %>%
    mutate(em15 = 100*((n_num_covid - n_num_other)/n_num_other),
           country = as.factor(country),
           country = relevel(as.factor(em$country), "Czech_Republic"))
  
  #Don't use this object for now--what does it mean to have the average excess mortality regime?
  em_mean <- death_rates %>%
    filter(age %ni% c("0-14", "all_sum", "all_mid")) %>%
    ungroup() %>%
    group_by(index) %>%
    summarize(n_num_covid = sum(n_num_covid, na.rm = T),
              n_num_other = sum(n_num_other, na.rm = T)) %>%
    mutate(em15 = 100*((n_num_covid - n_num_other)/n_num_other),
           country = "Average") %>%
    bind_rows(em) %>%
    mutate(country = as.factor(country),
           country = relevel(country, "Average"))
    
##Laying the groundwork
#1. Mortality rates

#Run ANOVA and Dunnett test
anova.em15 <- aov(em15 ~ country, data = em)
fit.dunnett.em15 <- glht(anova.em15, linfct = mcp(country = "Dunnett"))
summary(fit.dunnett.em15)

confint(fit.dunnett.em15) %>% 
  tidy %>% 
  mutate(country = gsub(" - .*$", "", contrast),
         country = gsub("_", " ", country)) %>%
  ggplot(aes(reorder(country, estimate), y=estimate, ymin=conf.low, ymax=conf.high)) +
  geom_hline(yintercept=0, linetype="11", colour="grey60") +
  geom_errorbar(width=0.1) + 
  #geom_text(aes(label = round(estimate, 0)), position = position_dodge(1), hjust = -7.5) +
  geom_point() +
  coord_flip() +
  labs(y = "Mean Difference from Reference", x = "Country") +
  theme_bw()  

#2. Kin structure

#This is more complicated, since need to look at each group within a country
#The first step is to determine whether we can get rid of some of this variation
#--is there a statistically significant difference between covid and other networks?

lm(count_all_covid-count_all_other ~ 0 + country, data = kin_structure) %>%
  tidy %>%
  filter(p.value < 0.05)

#Most countries we cannot reject the null that the difference is equal to zero
  

#Kin availability by age and sex
table(kin_structure$country[which(kin_structure$sex == "F" & kin_structure$age == "30-44")], 
      kin_structure$kintype[which(kin_structure$sex == "F" & kin_structure$age == "30-44")])

kin_structure$country <- as.factor(kin_structure$country)

anova.gparents.30F <- aov(count_all_covid ~ country, 
                          data = kin_structure[which(kin_structure$sex == "F" 
                                                     & kin_structure$age == "30-44" 
                                                     & kin_structure$kintype == "gparents"),])

#Load this library to run a Tukey test  
require(multcompView)
require(multcomp)

#Tukey test
tukey.gparents.30F <- TukeyHSD(x=anova.gparents.30F, 'country', conf.level=0.95)
plot(tukey.gparents.30F , las=1 , col="brown")

fit.dunnett.gparents.30F <- glht(anova.gparents.30F, linfct = mcp(country = "Dunnett"))
summary(fit.dunnett.gparents.30F)


#Create a mean country that we can compare to others
kin_structure_mean <- kin_structure %>%
  mutate(country = as.character(country)) %>%
  filter(index <= 98) %>%
  group_by(index, age, sex, kintype) %>%
  summarize(count_all_covid = mean(count_all_covid, na.rm = T),
            count_all_other = mean(count_all_other, na.rm = T),
            count_male_covid = mean(count_male_covid, na.rm = T),
            count_male_other = mean(count_male_other, na.rm = T),
            count_female_covid = mean(count_female_covid, na.rm = T),
            count_female_other = mean(count_female_other, na.rm = T),
            count_65plus_covid = mean(count_65plus_covid, na.rm = T),
            count_65plus_other = mean(count_65plus_other, na.rm = T),
            count_below65_covid = mean(count_below65_covid, na.rm = T),
            count_below65_other = mean(count_below65_other, na.rm = T),
            n_egos_covid = mean(n_egos_covid, na.rm = T),
            n_egos_other = mean(n_egos_other, na.rm = T)) %>%
  mutate(country = "Average") %>%
  bind_rows(kin_structure) %>%
  mutate(country = as.factor(country),
         country = relevel(country, "Average"))

anova.gparents.30F.mean <- aov(count_all_covid ~ country, 
                               data = kin_structure_mean[which(kin_structure_mean$sex == "F" 
                                                               & kin_structure_mean$age == "30-44" 
                                                               
                                                               & kin_structure_mean$kintype == "gparents"),])
fit.dunnett.gparents.30F.mean <- glht(anova.gparents.30F.mean, linfct = mcp(country = "Dunnett"))
summary(fit.dunnett.gparents.30F.mean)
plot(fit.dunnett.gparents.30F.mean, las=1 , col="brown")


