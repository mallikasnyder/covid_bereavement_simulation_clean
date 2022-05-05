# This script has some basic functions for recovering
# input fertility and mortality age-specific rates that were
# used to create the simulation. Extracting these rates and
# comparing them to the input simulation is considered good practice
# as it allows us to assess the accuracy of our microsimulation.
# Note that this is only one of multiple checks that can be performed
# on the opop population files.



# 1. Load functions and data --------
rm(list=ls())

library(tidyverse)
source("~/covid_simulation/Kin_death/functions_socsim.R")
source("~/covid_simulation/Kin_death//01_load_functions.R")

countries <- list.files("~/90days/covidRESULTS/SocCovid-mallikasnyder/")

calibrateRates <- function(country){

sim.ids <- grep(list.files(paste0("~/90days/covidRESULTS/SocCovid-mallikasnyder/", country, "/")), pattern = ".opop", invert = T, value = T)

opops <- bind_rows(lapply(1:length(sim.ids), function(i){
opop <- 
  read_sweden_socsim(path=paste0(paste0("~/90days/covidRESULTS/SocCovid-mallikasnyder/", country, "/"), sim.ids[i], "/SimResults/sims.Rsave")) %>% 
  select(pid, mom, pop, fem, birth_year, death_year, dob, dod) %>% 
  # fem = 1 for women!
  # Remove founder generation (who have no mother)
  # People can still have no dads but that's fine because
  # Children of  unmarried couples get a 0 as id for their 
  # father by default.
  filter(!is.na(mom)) %>% 
  # And let's just start from 1751, which is the first period for which 
  # we provided empirical rates
  filter(birth_year >= 1751) %>%
  mutate(sim.id = sim.ids[i])
}))


# 2. Define some parameters ========


y_min <- 1950
y_max <- 2035
y_range <- y_min:y_max

y_breaks <- seq(y_min, y_max, 5)

# lower age bounds of age categories of mortality rates
age_breaks_mort <- c(0, 1, seq(5, 100, by = 5))
age_labels_mort <- age_breaks_mort[-length(age_breaks_mort)]

age_group_size <-  5
min_age <-  15
max_age <-  50

# For fertility rates

age_group_size <-  5
min_age <-  15
max_age <-  50

age_breaks_fert <- seq(min_age, max_age, by = age_group_size)
age_labels_fert <- age_breaks_fert[-length(age_breaks_fert)]

# 3. Extract mortality and fertility rates ------
asmrs <- bind_rows(lapply(1:length(sim.ids), function(i){
asmr <- 
  get_asmr_socsim_period(
    df = opops %>% filter(sim.id == sim.ids[i])
    , age_breaks = age_breaks_mort
    , age_labels = age_labels_mort
    , y_breaks = y_breaks
    , un_y_labs = NA
    , y_range = y_range
    , compare_to_un = F
    , only_women = F
  )
asmr$sim.id <- rep(sim.ids[i], nrow(asmr))
return(asmr)
}))

asfrs <- bind_rows(lapply(1:length(sim.ids), function(i){
asfr <- 
  get_asfr_socsim(
    df = opops %>% filter(sim.id == sim.ids[i])
    , sex_keep = "female"
    , y_range = y_range
    , age_breaks = age_breaks_fert
    , age_labels = age_labels_fert
  )
asfr$sim.id <- rep(sim.ids[i], nrow(asfr))
return(asfr)
}))

asmr <- asmrs %>%
  ungroup() %>%
  group_by(year, gender, age) %>%
  summarize(socsim = mean(socsim, na.rm = T))

asfr <- asfrs %>%
  ungroup() %>%
  group_by(year, age) %>%
  summarize(socsim = mean(socsim, na.rm = T))

# 3. Plot recovered SOCSIM -------

bind_rows(
  asmr %>% 
    mutate(name = "asmr") %>% 
    filter(gender == "female")
  , asfr %>% 
    mutate(
      name = "asfr"
      , age = as.numeric(as.character(age))
    )
) %>% 
  ggplot(aes(x = age, y = socsim, colour = year, group = year)) +
  geom_line() +
  facet_wrap(. ~ name, scales = "free") + 
  theme_bw()

ggsave(filename = paste0("~/covid_simulation/Data/checking_rates/", gsub(".sup", "", country), ".png"))

print(paste(country, "completed!"))
}

calibrated <- lapply(1:length(countries), function(x) calibrateRates(country = countries[x]))
