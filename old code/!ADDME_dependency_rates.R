# ---------------------------------------------------------------------------------
# Please add hese functinos to functions_bereavement.R -----------

# This can be merged with untion in global
# Get psuedo census of population alive in given year or period
# from genealogical data
census_socsim <- function(df, y, return_ids = T, group_by_sex = F, group_by_age_sex = F) {
  
  df$census <- y
  
  df$death_temp <- df$death
  
  year_df <- 
    df %>% 
    dplyr::filter(birth <= y & death_temp >= y)
  
  if(return_ids) {
    out <- year_df$profileid
  } else {
    if(group_by_sex) {
      out <- year_df %>% 
        dplyr::count(gender, census)
    } else if(group_by_age_sex) {
      out <- year_df %>% 
        dplyr::mutate(
          age_at_census = census - birth
        ) %>% 
        dplyr::count(gender, age_at_census, census) %>% 
        tidyr::complete(gender, age_at_census, census, fill = list(n = 0))
    }
    else{
      out <- year_df
    }
    
  }
  
  return(out)
  
}

# Get dependency rate, young dependency and old dependency rates given an opop
# and a vector of period years
get_dependency_ratio <- function(
  df, type = "dependency", tau = 5,
  year_range,
  sim.id, country, scenario,
  FinalSimYear, endmo
  , feb2020, postcovid2020
){
  
  # Added on 20201030 by diego
  # We want to get dependency rates to use as a country-level indicator
  # in the actual analysis of excess bereavement (ie the regression)
  # For this, we need to get, for each simulation, one value per type of 
  # dependency ratio.
  # We are only interested in the dependency ratio at the onset of the pandemic. 
  # For convenience, let us estimate the dependency ratios for 2019, one year before
  # the pandemic:
  
  # jan_2019 <-  feb2020 - 13
  # dec_2019 <- jan_2019 + 11
  
  # We estimate two types of dependency ratio:
  # 1. Old-age
  # 2. Prospective
  
  # But first, some data edits
  
  opop2 <- 
    df %>% 
    mutate(
      dod2 = ifelse(dod == 0, endmo, dod)
      , death_year = asYr(dod2, FinalSimYear, endmo)
      , birth_year = asYr(dob, FinalSimYear, endmo)
    ) %>% 
    rename(gender = fem) %>% 
    select(pid, birth = birth_year, death = death_year, gender)
  
  # Iterate each year
  all_ages <- 
    lapply(year_range, census_socsim, df = opop2, return_ids = F, group_by_age_sex = T) %>% 
    do.call(rbind, .) %>% 
    data.frame() %>% 
    rename(fem = gender, pop = n)
  
  # Now implement type of dependency rate
  
  # 1. Dependency ratio ~~~~~~~~~~~~~~~~~~~~
  # Numerator:people younger than 15 and older that 64; 
  # denominator: between 15 and 64 years old. 
  
  dependency <- 
    all_ages %>% 
    mutate(
      dependent = ifelse(age_at_census < 15 | age_at_census > 64, T, F)
    ) %>% 
    group_by(census, dependent) %>% 
    summarise(n = sum(pop)) %>% 
    ungroup() %>% 
    arrange(census, dependent) %>% 
    mutate(dependency = n / lag(n)) %>% 
    filter(dependent) %>% 
    select(-dependent, - n)
  
  # 2. Young dependency ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Numerator: younger than 15 years old. 
  # Dennominator: 15-64
  
  
  young_dependency <- 
    all_ages %>% 
    mutate(
      dependent = ifelse(age_at_census < 15, T, F)
      , dependent = ifelse(age_at_census >= 15 & age_at_census < 65, F, dependent)
      , dependent = ifelse(age_at_census > 65, NA, dependent)
    ) %>% 
    filter(!is.na(dependent)) %>% 
    group_by(census, dependent) %>% 
    summarise(n = sum(pop)) %>% 
    ungroup() %>% 
    arrange(census, dependent) %>% 
    mutate(young = n / lag(n)) %>% 
    filter(dependent) %>% 
    select(-dependent, - n)
  
  
  # 3. Old dependency ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Numerator older than 64 years.  
  # Dennominator: 15-64
  
  old_dependency <- 
    all_ages %>% 
    mutate(
      dependent = ifelse(age_at_census > 64, T, F)
      , dependent = ifelse(age_at_census >= 15 & age_at_census < 65, F, dependent)
      , dependent = ifelse(age_at_census < 15, NA, dependent)
    ) %>% 
    filter(!is.na(dependent)) %>% 
    group_by(census, dependent) %>% 
    summarise(n = sum(pop)) %>% 
    ungroup() %>% 
    arrange(census, dependent) %>% 
    mutate(old = n / lag(n)) %>% 
    filter(dependent) %>% 
    select(-dependent, - n)

  # 4. Prospective old-age dependency ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Numerator: people expected to die within tau years.
  
  # I can't use the previous script since I need individual-level censu data
  prospective <- 
    lapply(year_range, census_socsim, df = opop2, return_ids = F, group_by_age_sex = F) %>% 
    do.call(rbind, .) %>% 
    data.frame() %>% 
    rename(fem = gender) %>% 
    mutate(
      age_at_census = census - birth
      , doomed = ifelse(death_temp - tau <= census, T, F)
      , working = ifelse(age_at_census >= 15 & age_at_census < 65, T, F)
      , working = ifelse(doomed, F, working)
      , dependent = !working
      , kids = ifelse(age_at_census < 15, T, F)
    ) %>% 
    filter(!kids) %>% 
    # Get number of cases by census and age and STATUS
    dplyr::count(census, age_at_census, dependent) %>% 
    tidyr::complete(census, age_at_census, dependent, fill = list(n = 0)) %>% 
    rename(pop = n) %>% 
    group_by(census, dependent) %>% 
    summarise(n = sum(pop)) %>% 
    ungroup() %>% 
    arrange(census, dependent) %>% 
    mutate(prospective = n / lag(n)) %>% 
    filter(dependent) %>% 
    select(-dependent, - n)
  
  # Put all together
  output <- 
    dependency %>% 
    left_join(young_dependency, by = "census") %>%
    left_join(old_dependency, by = "census") %>% 
    left_join(prospective, by = "census") %>% 
    rename(year = census) %>% 
    mutate(sim.id = sim.id, country = country, scenario = scenario)
  
  output
    
}

# ---------------------------------------------------------------------------------
# This is the actual function that should be added to getEstimatesNew() -----------

dep_ratio <- get_dependency_ratio(
  df = opop
  , tau = 5,
  year_range = 2018:2020
  , sim.id = sim.id
  , country = country
  , scenario = scenario
  , FinalSimYear = FinalSimYear
  , endmo = endmo
)
