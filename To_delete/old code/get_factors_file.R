#Loading the packages needed
require(tidyverse)
require(data.table)
require(countrycode)
require(splitstackshape)

#Values used later (code based on MakeFactor.R)
stmf <- fread("~/covid_simulation/Rcode/stmf-mar9-run.csv", stringsAsFactors = F)
names(stmf) <- tolower(names(stmf)) #Make names lowercase
final_week <- 24

#Keeping only Sweden
keepcountry <- "SWE"

#Pivoting the STMF data into long format
adj.total <- stmf %>%
  dplyr::select(-c(split, splitsex, forecast)) %>% #removing variables
  filter(countrycode %in% keepcountry) %>%
  pivot_longer(-c(year, week, countrycode, sex), 
               names_to = c("type", "age"), values_to = "rate", names_sep = 1) %>% #pivoting
  pivot_wider(names_from = "type", values_from = "rate") %>%
  mutate(death_count = d, #renaming and generating new versions of variables
         death_rate = r,
         age = gsub("85p", "85-99", gsub("_", "-", age)),
         country = countrycode,
         sex = if_else(sex %in% c("f"), "Female", if_else(sex %in% c("m"), "Male", "b"))) %>%
  filter(age != "total", between(year, 2016, 2020), sex != "b") %>%
  mutate(exposure = death_count/death_rate) %>%
  group_by(country, sex, age, year) %>%
  mutate(exposure2 = max(exposure, na.rm = T), #this accounts for the few cases where a weekly exposure is 0 due to no deaths in a group
         exposure_fixed = if_else(is.na(exposure), exposure2, exposure)) %>%
  dplyr::select(-c(exposure, exposure2, d,r, countrycode)) %>% #removing provisional and unnecessary variables variables
  rename(exposure = exposure_fixed) #specifying final exposure variable

#Now convert weekly data into monthly data
adj.monthly <- adj.total %>%
  filter(week <= final_week) %>% #removing all data after a set cutoff point for comparability between countries
  mutate(month = as.numeric(as.factor(cut(week, breaks = seq(1, final_week+1, by = 4), 
                                          right = FALSE)))) %>%
  ungroup() %>%
  group_by(country, year, month, sex, age) %>%
  summarize(death_count = sum(death_count), exposure = sum(exposure), .groups = "keep") %>% #generating monthly death counts and exposures
  mutate(period = if_else(year %in% c(2016:2019), "Past", "Current"))

#Now we can generate factors, depending on the method selected
  adj.past <- adj.monthly %>%  #Calculate average for past 4-year period
    filter(period == "Past") %>%
    ungroup() %>%
    group_by(country, month, sex, age) %>%
    summarize(mean_past_deaths = mean(death_count), mean_past_exposure =  mean(exposure),
              mean_past_mx = mean_past_deaths/mean_past_exposure, .groups = "keep")
  
  adj.current <- adj.monthly %>% filter(period == "Current") %>% #Find current mx values
    ungroup() %>%
    mutate(current_death_count = death_count, current_exposure = exposure,
           current_mx = current_death_count/current_exposure) %>% 
    dplyr::select(-c(year, death_count, exposure, period))
  
  #Getting final monthly factors
  adj.monthly.final <- left_join(adj.past, adj.current, #Combine past and current values
                                 by = c("country" = "country", "month" = "month", "sex" = "sex", "age" = "age")) %>%
    mutate(mean_past_mx = if_else(mean_past_mx == 0, 10^-6, mean_past_mx),
           mx_ratio = current_mx/mean_past_mx) %>% #Calculate adjustment factor
 mutate(country_name = ifelse(country == "TWN", "China Taiwan Province of China", #To match UN rates files
                                 countrycode(substr(country, 1, 3), origin = "iso3c", destination = "un.name.en")),
           country_name = c(gsub(" ", "_", 
                                 gsub("United Kingdom of Great Britain and Northern Ireland",
                                      "United Kingdom", country_name)))) 
  
write_csv(adj.monthly.final, "~/covid_simulation/Data/adjfactors-mar9.csv")
