#Load functions
#Setting directories and loading functions
setwd("~/covid_simulation/")

#Source functions and packages
source("~/covid_simulation/Kin_death/01_load_functions.R")
source('~/covid_simulation/Kin_death/functions_bereavement.R')

#Calculate excess mortality rates by country over the period
#setting parameters
lastweek <- 24
same_week <- T

#Load STMF data
stmf <- fread("../Data/stmf_sep8.csv", stringsAsFactors = F)
names(stmf) <- tolower(names(stmf)) #Make names lowercase

#Dropping countries that do not have data for the last week
dropcountry <- stmf %>% 
  filter(year == 2020) %>% 
  group_by(countrycode) %>% 
  summarize(lastweek = max(week)) %>%
  mutate(lastmonth = trunc(lastweek/4), lastweek_trunc = lastmonth*4) %>%
  filter(lastweek_trunc < 20) %>%
  pull(countrycode)

#Pivoting the STMF data into long format
adj.total <- stmf %>%
  select(-c(split, splitsex, forecast)) %>% #removing variables
  pivot_longer(-c(year, week, countrycode, sex), 
               names_to = c("type", "age"), values_to = "rate", names_sep = 1) %>% #pivoting
  pivot_wider(names_from = "type", values_from = "rate") %>%
  mutate(death_count = d, #renaming and generating new versions of variables
         death_rate = r,
         age = gsub("85p", "85-99", gsub("_", "-", age)),
         country = countrycode,
         sex = if_else(sex %in% c("f"), "Female", if_else(sex %in% c("m"), "Male", "b"))) %>%
  select(-c(d,r, countrycode)) %>%
  filter(age != "total" & year >= 2016 & sex != "b", country != dropcountry) %>% #removing countries with data that does not extend to May
  mutate(exposure = death_count/death_rate) %>%
  group_by(country, sex, age, year) %>%
  mutate(exposure2 = max(exposure, na.rm = T), #this accounts for the few cases where a weekly exposure is 0 due to no deaths in a group
         exposure_fixed = if_else(is.na(exposure), exposure2, exposure)) %>%
  select(-c(exposure, exposure2)) %>% #removing provisional variables
  rename(exposure = exposure_fixed) #specifying final exposure variable

#STMF currently reports the UK estimate for England and Wales and Scotland separately; we combine them into one country and use that estimate for the UK
#Seeing if we need to adjust the UK estimate: we see if we have two countries containing the code GBR
adjustUK <- if_else(length(grep("GBR", levels(as.factor(adj.total$country)))) > 1, T, F)

#Print whether we need to adjust this
print(paste("UK estimate being constructed from", length(grep("GBR", levels(as.factor(adj.total$country)))), "entities"))

if (adjustUK <- T) {
  #England and Wales and Scotland may go up to different number of weeks. Make sure we have the same weeks.
  week_final_uk <-  adj.total %>% 
    filter(grepl("GBR", country), year == 2020) %>%
    ungroup() %>%
    group_by(country) %>%
    summarize(max_week = max(week))
  
  #Checking the final week matches
  for (i in 1:(length(week_final_uk$max_week)-1)) {
    same_week_uk <- (week_final_uk$max_week[i] == week_final_uk$max_week[i+1])
  } 
  
  print(paste0("UK estimates have the same final week: ", same_week_uk))
  
  if(same_week_uk){
    adj.total.uk <- adj.total %>% 
      filter(grepl("GBR", country)) %>%
      ungroup() %>%
      group_by(year, week, sex, age) %>%
      summarize(death_count = sum(death_count), exposure = sum(exposure)) %>% #Adding up UK estimates
      mutate(country = "GBR") #Creating one country code
    
    adj.total2 <- adj.total %>% 
      filter(!grepl("GBR", country)) %>% #Combining them with all other countries in the dataset
      bind_rows(adj.total.uk)
  }
  else{ #This is just in case we have different final weeks
    last_week_uk <- min(week_final_uk$max_week)
    
    adj.total.uk <- adj.total %>% 
      filter(grepl("GBR", country), year<2020|year==2020 & week <= last_week_uk) %>%
      ungroup() %>%
      group_by(year, week, sex, age) %>%
      summarize(death_count = sum(death_count), exposure = sum(exposure)) %>% #Adding up UK estimates
      mutate(country = "GBR") #Creating one country code
    
    adj.total2 <- adj.total %>% 
      filter(!grepl("GBR", country)) %>% #Combining them with all other countries in the dataset
      bind_rows(adj.total.uk)
  }
}

#Load countrycode package
require(countrycode)

#Calculate mean differences by country
stmf.em <- adj.total2 %>%
  filter(between(week, 9, lastweek), age!= "0-14") %>% #removing all data after a set cutoff point for comparability between countries
  ungroup() %>%
  group_by(country, year) %>%
  summarize(death_count = sum(death_count), exposure = sum(exposure)) %>% #generating period death counts and exposures
  mutate(period = if_else(year %in% c(2016:2019), "past", "current")) %>%
  ungroup() %>%
  group_by(country, period) %>%
  summarize(mean_death_count = mean(death_count), mean_exposure = mean(exposure), mx = 1000*(mean_death_count/mean_exposure)) %>%
  select(-c(mean_death_count, mean_exposure)) %>%
   pivot_wider(names_from = "period", values_from = c("mx")) %>%
  ungroup() %>%
  mutate(mx_relative_diff = 100*((current-past)/past),
         country_name = countrycode(substr(country, 1, 3), origin = "iso3c", destination = "un.name.en"), #Using the countrycode package to find country names
         country_name = c(gsub(" ", "_", 
                               gsub("United Kingdom of Great Britain and Northern Ireland", #Adjusting the UK's name to match what we use later in our .opop file 
                                    "United Kingdom", country_name))),
         country_code = country, country = country_name, country_name = NULL)

#Calculate excess bereavement (using same method as in first analysis)
data<- get(load(file = "../Data/final_data_paa.RData"))
#Get the burden of bereavement object

data2 <-data$kin_bb %>%
  filter(!is.na(scenario)) %>% 
  mutate(
    sex = ifelse(grepl("f1", category), "F", "M")
    , age = gsub("f[0-1]{1}", "", category)
  ) %>% 
  # select(country, sim.id, scenario, age, sex, kintype, mean_pre, mean_post) %>% 
  # Reduce simulations to one obs per country
  # group_by(country, scenario, age, sex, kintype) %>%
  group_by(country, scenario, kintype) %>% #All-ages
  summarise(
    mean_pre = mean(mean_pre, na.rm = T) 
    , mean_post = mean(mean_post, na.rm = T)
  ) %>% 
  ungroup() 


# 2. Get excess bereavement rate ============
# Positive values means that more kin loss in covid scenario
ebr <-
  data2 %>% 
  select(scenario, kintype, country, value = mean_post) %>%
  # select(scenario, kintype, country, age, sex, value = mean_post) %>% 
  pivot_wider(
    names_from = scenario
    , values_from = value
  ) %>%
  mutate(
    #value = ((covid-other)/other) *100
    value = (other-covid)/covid *100
    , value = ifelse(is.nan(value)|is.infinite(value), 0, value)
    , value = round(value, 1)
  ) %>% 
  select(kintype, country,  ebr = value)
# select(kintype, age, country,  sex, value)

#Merge ebr and STMF adjustment factors
plot_data <- left_join(ebr, stmf.em, by = "country")

require(ggrepel)
#Make correlation plot
plot_data %>%
  ungroup() %>%
  filter(kintype %in% c("nuclear", "extended", "gparents", "parents"), 
         country != "Czech_Republic") %>%
  mutate(country = gsub("United_States_of_America", "USA", gsub("United_Kingdom", "UK", country)),
         country_code = gsub("GBR", "UK", substr(country_code, 1, 3)),
         kintype = if_else(kintype == "nuclear", "Nuclear Family",
                  if_else(kintype == "extended", "Extended Family",
                  if_else(kintype == "gparents", "Grandparents",
                  if_else(kintype == "parents", "Parents", kintype)))),
         kintype = factor(kintype, ordered = T, levels = c("Nuclear Family", "Extended Family", "Grandparents", "Parents")  )) %>%
  ggplot(aes(x = mx_relative_diff, y = ebr)) +  geom_smooth(method = "lm", se = T, color = "gray47", fill = "lightgray") +
  geom_point() +
  geom_text_repel(data = (. %>% filter(country %in% c("USA", "UK", "Spain", "France", "Italy", "Germany", "Sweden"))), 
            aes(label = country), color = "gray47") +
  #data = (. %>% filter(mx_relative_diff %in% range(mx_relative_diff))
  facet_wrap(~kintype)+ theme_bw() +
  labs(x = "Excess Mortality, Ages 15+ (%)", y = "Excess Bereavement, All Ages (%)"
       # , title = "Excess Mortality and Excess Bereavement by Kin Relationship"
       ) +
  theme(text = element_text(size=16))

#Save the graph
ggsave(file = "../Output/paa_fig1_final.pdf", width = 20, height = 12, units = "cm")

#width=1200&height=482


#~~~~~~~~~~EXTRA CODE (DON'T RUN): CHECKING THE EXCESS MORTALITY RATES LINE UP REASONABLY WELL
source('~/covid_simulation/Rcode/MakeFactor.R')

testfactor <- estimateFactor(source = "../Data/stmf_sep8.csv", lastweek = 24, same_week = T)
simfactor <- get(load(file = "../Data/STMF_factor_latest_sim.RData"))

checking.factors <- tibble(factor.test = testfactor, factor.sim = simfactor, 
                           country.test = attr(testfactor, "country"), country.sim = attr(simfactor, "country"),
                           month.test = attr(testfactor, "month"), month.sim = attr(simfactor, "month"),
                           sex.test = attr(testfactor, "sex") , sex.sim = attr(simfactor, "sex"),
                           age.test = attr(testfactor, "age"), age.sim = attr(simfactor, "age") 
)

checking.factors2 <- checking.factors %>%
  filter(factor.test != factor.sim)
