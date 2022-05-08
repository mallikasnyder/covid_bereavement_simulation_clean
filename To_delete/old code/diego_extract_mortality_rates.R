
# Description:
# Given an opop, save to the disk, for 2020:
# - crude death rate
# - age-specific mortality rates

# Sugested placement ------------------------
# Add this in line 715 of functions_bereavement.R, after having loaded an opop
# (inside function getSimEstimates())

# Parameteres ------------------------
# sim.id ... should be in function environemtn
# FinalSimYear  ... should be in function environemtn
# endmo  ... should be in function environemtn
# feb2020  ... should be in function environemtn

age_br_months <- c(0, 14, 44, 64, Inf)*12
age_labs <- c("0-14", "15-44", "45-64", "65+")
path_out <- ".../.../Output/" # NEEDS TO BE DEFINED! Where will death rates text files saved?

# FUNCTION ----------------------

save_mortality_rates_first_wave_to_disk <- function(df, get_death_rate = T, get_asmr = T, age_br_months, age_labs, FinalSimYear, endmo, feb2020, path_out){
  
  # From socsim opop file
  # Do all estimates in months!
  # Feb 2020 is feb2020 = 3241
  # feb2020 should exist already 
  
  # Returns no value!
  
  # This estimates the mortality rate for the entire year
  # month_range <- c(feb2020 - 1, feb2020 + 10)
  # This estimates the mortality rate for Feb - to the end of the pandemic
  # assumed to be August 2020
  month_range_low <- feb2020 - 1
  month_range_high <- feb2020 + 6

  mid_period_month <- trunc(month_range_low + diff(c(month_range_low, month_range_high))/2)
  
  # A dataframe of people alive at start of period
  alive_at_start <- 
    df %>% 
    mutate(
      dod2 = ifelse(dod == 0, endmo, dod)
      , age_death_months = dod2-dob #ifelse(dod == 0,NA,dod-dob)
      , age_mid_period_months = mid_period_month-dob #ifelse(dod == 0,NA,mid_period_month-dob)
    ) %>% 
    select(dob, dod2, fem, age_death_months, age_mid_period_months) %>% 
    mutate(
      age_death_gr = cut(age_death_months, age_br_months, age_labs, include.lowest = T)
      , age_mid_period_gr = cut(age_mid_period_months, age_br_months, age_labs, include.lowest = T)
      ) %>% 
    dplyr::filter(dob <= month_range_low & dod2 >= month_range_low)
  
  # DEATH RATE
  if(get_death_rate){
    
    deaths_vector <- alive_at_start$dod2
    
    mid_period_pop <- sum(deaths_vector >= mid_period_month)
    
    # This is already a vector born before start of period
    # and who died beore of period, so just need
    # to filter to keep those that died in the given interval
    deaths_period <- sum(deaths_vector <= month_range_high)
    
    death_rate <- deaths_period/mid_period_pop * 1000

    # Export 
    write(x = death_rate, paste0(path_out, "death_rates_", sim.id, ".txt"))
    
  }
  
  # Get Age-specific mortality rate for given age groups
  if(get_asmr){
    
    # Numerator - death counts by age and sex
    
    numerator <- 
      alive_at_start %>% 
      # keep only those that died before end of period
      filter(dod2 <= month_range_high) %>% 
      dplyr::count(fem, age = age_death_gr) %>% 
      tidyr::complete(fem, age, fill = list(n = 0)) %>% 
      arrange(fem, age)
    
    # Denominator 
    
    # Currently, this is the age-sex distribution of people who were alive at the start of the period
    # considering their age at the middle of the period. 
    # Now, it only considers the year of death, meaning that the poulation includes all those who were alive 
    # at the start of a given year and did not die that year.
    # This is consistent with HMD's definition of "alive on January 1st" but could still be corrected
    # to account for "the timing of deaths during the interval and variation in cohort's birthdays by month"
    
    denominator <-
      alive_at_start %>%
    # MS: Changing the filtering to remove those who never died
     # filter(dod2 < endmo) %>% 
      dplyr::count(fem, age = age_mid_period_gr) %>% 
      tidyr::complete(fem, age, fill = list(n = 0)) %>% 
      arrange(fem, age)
    
    # Rate
    asmr <- 
      numerator %>% 
      left_join(denominator, by = c("fem", "age"), suffix = c("_num", "_den")) %>% 
      mutate(
        value = n_num / n_den
        , value = ifelse(is.na(value), 0, value) * 1000
      ) 
    
    write.csv(x = asmr, paste0(path_out, "asmr_", sim.id, ".csv"), row.names = F)
  }
  
  print("Death rates saved to disk (function returns no object)")
  
}


# Test ------------
# df <- opop <- socsim_opop_USA_246306 <- read.csv("C:/Cloud/Projects/sandwich/Data/socsim/socsim_opop_USA_246306.csv")
 path_out <- "~/covid_simulation/Output/"
 sim.id <- "test"

test <- save_mortality_rates_first_wave_to_disk(
  df = opop
  , sim.id = sim.id
  , get_death_rate = T
  , get_asmr = T
  , age_br_months = age_br_months
  , age_labs = age_labs
  , FinalSimYear = FinalSimYear
  , endmo = endmo
  , feb2020 = feb2020
  , postcovid2020 = postcovid2020
  , path_out = path_out
)
