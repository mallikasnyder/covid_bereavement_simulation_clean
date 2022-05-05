# SOCSIM ==============

asYr_hmd <-function(x,lastmo=endmo,FinalSimYear){
  ## convert sim month to a real calendar year
  ## handy for graphing mainly.
  ## requires that FinalSimYear be set in the GlobalEnv
  # stopifnot("FinalSimYear" %in% objects())
  yr<-ifelse(x==lastmo,FinalSimYear,
             FinalSimYear - trunc((lastmo -x )/12))
  return(yr)
}

read_sweden_socsim <- function(path){
  # Read Swedish simulation with 
  # historical reates as input.
  # This is the socsim simulation 
  # created by emilio and that I am
  # repurposing here
  load(path)
  
  opop <- sims$opop  
  
  ## assign names to columns
  names(opop)<-c("pid","fem","group",
                 "nev","dob","mom","pop","nesibm","nesibp",
                 "lborn","marid","mstat","dod","fmult")
  
  FinalSimYear<-2035  ## check .sup file and give this careful consideration
  #endmo<-7741 ##6541  #5307  ## last month of simulation See Socsim
  ##output
  endmo<-max(opop$dob)  #+1
  EndYr<-endmo:(endmo-11)
  
  opop <- opop %>% mutate(dod = ifelse(dod == 0 & dob > 0, endmo, dod))
  
  ##Include year of birth and year of death into the pop file
  opop$birth_year <- asYr_hmd(opop$dob,endmo,FinalSimYear)
  opop$death_year <- asYr_hmd(opop$dod,endmo,FinalSimYear)
  
  return(opop)
}

# Functions to extract rates =========

get_asmr_socsim_period <- function(df, age_breaks, age_labels, y_breaks, un_y_labs, y_range, compare_to_un = T, only_women = F){
  
  opop2 <- 
    df %>% 
    mutate(
      # dod2 = ifelse(dod == 0, endmo, dod)
      # , death_year = asYr_hmd(dod2, FinalSimYear, endmo)
      # , birth_year = asYr_hmd(dob, FinalSimYear, endmo)
      age_death_months = ifelse(dod == 0,NA,dod-dob)
      , age_death = trunc(age_death_months/12)
      , age_death_g = cut(age_death, breaks = age_breaks, labels = age_labels, include.lowest=TRUE,right=FALSE)
      , age_death_g = as.numeric(as.character(age_death_g))
    ) %>% 
    rename(gender = fem)
  
  # 1.1. Numerator - death counts 
  
  # drop the folks in the initial population
  # agedC is aged at death OR censor
  # agedCf is a factor with the age cats used in ratefile.min
  
  numerator <- 
    opop2 %>% 
    # filter(dob >= firstmonth) %>% 
    filter(dod != 0) %>% 
    dplyr::count(death_year, gender, age_death = age_death_g) %>% 
    select(year = death_year, age = age_death, everything()) %>% 
    filter(year %in% y_range) %>% 
    arrange(year, gender, age)
  
  # 1.2 Denominator 
  
  # Currently, this is poulation who was ever alive during the year
  # Now, it only considers the year of death, meaning that the poulation includes all those who were alive 
  # at the start of a given year and did not die that year.
  # This is consistent with HMD's definition of "alive on January 1st" but could still be corrected
  # to account for "the timing of deaths during the interval and variation in cohort's birthdays by month"
  
  opop2_subset <- 
    opop2 %>% 
    select(pid, birth = birth_year, death = death_year, gender)
  
  yearly_pop_age_sex <- lapply(y_range, census_socsim
                               , df = opop2_subset 
                               , return_ids = F
                               , group_by_sex = F
                               , group_by_age_sex = T
  )  
  
  denominator <- 
    data.frame(do.call(rbind, yearly_pop_age_sex)) %>% 
    dplyr::filter(!is.na(gender)) %>% 
    dplyr::select(year = census, everything()) %>% 
    dplyr::mutate(
      age = cut(age_at_census,breaks = age_breaks, labels = age_labels,include.lowest=TRUE,right=FALSE)
      , age = as.numeric(as.character(age))
    ) %>% 
    dplyr::group_by(year, gender, age) %>% 
    dplyr::summarise(n = sum(n)) %>% 
    arrange(year, gender, age)
  
  # 2.2.3. Rate
  
  asmr <- 
    numerator %>% 
    full_join(denominator, by = c("year", "gender", "age"), suffix = c("_num", "_den")) %>% 
    mutate(
      socsim = n_num / n_den
      , socsim = ifelse(is.na(socsim), 0, socsim)
      # Group by 5-years
      , yg = cut(year, breaks = y_breaks, include.lowest = T)
      , yg = as.character(yg)
      , gender = ifelse(gender == 0, "male", "female")
    ) %>% 
   group_by(year = yg, gender, age) %>% 
    summarise(
      socsim = mean(socsim)
    ) %>% 
    ungroup 
  
  # 1.3. Compare to UN WPP values 
  
  if(only_women){
    asmr <- asmr %>% 
      filter(gender == "female") 
  }
  
  
  return(asmr)  
}


get_asfr_socsim <- function(df, sex_keep, y_range, age_breaks, age_labels) {
  
  opop2 <- 
    df %>% 
    # filter(between(birth_year, min(y_range), max(y_range))) %>% 
    mutate(gender = ifelse(fem == 0, "male", "female"))
  
  # 1. Denominator - women in reproductive years
  den <- lapply(y_range, get_women_reproductive_age_socsim
                , df = opop2
                , age_breaks = age_breaks
                , age_labels = age_labels
                , sex_keep = "female"
  ) 
  
  denominator <- data.frame(do.call(rbind, den))
  
  # 2. Enumerator - births to women in given age group
  
  enumerator <- yearly_birth_by_age_socsim(opop2, sex_keep, y_range,age_breaks, age_labels)
  
  # 3. Rate
  
  asfr <- 
    bind_cols(denominator %>% rename(den = n),
              enumerator %>% select(enu = n)) %>% 
    dplyr::mutate(
      socsim = enu/den
      , socsim = ifelse(is.na(socsim), 0, socsim)
      # Group by 5-years
      , yg = cut(year, breaks = y_breaks,include.lowest = T)
      , yg = as.character(yg)
      , age = agegr
    ) %>% 
    group_by(year = yg, age = agegr) %>% 
    summarise(
      socsim = mean(socsim)
    ) %>% 
    ungroup 
  
  return(asfr)
  
}

get_women_reproductive_age_socsim <- function(df, year, age_breaks, age_labels, sex_keep = "female") {
  
  df$census <- year
  
  df$death_temp <- df$death_year
  
  out <-
    df %>% 
    dplyr::filter(birth_year <= year & death_temp >= year) %>% 
    dplyr::filter(gender %in% sex_keep) %>%  
    dplyr::mutate(
      age_at_census = census - birth_year
      , agegr_at_census = cut(age_at_census, age_breaks, labels = age_labels, include.lowest = TRUE, 
                              right = F)
      , agegr_at_census = as.character(agegr_at_census)
    ) %>% 
    dplyr::filter(!is.na(agegr_at_census)) %>% 
    dplyr::count(agegr_at_census, census) %>% 
    mutate(
      agegr_at_census = factor(agegr_at_census, levels = age_labels)
      # , census = factor(census, levels = y_range)
    ) %>% 
    tidyr::complete(agegr_at_census, fill = list(n = 0)) %>% 
    select(year = census, agegr = agegr_at_census, n) %>% 
    arrange(year, agegr)
  
  return(out)
}

yearly_birth_by_age_socsim <- function(df, sex_keep, y_range, age_breaks, age_labels) {
  
  col <- ifelse(sex_keep == "male", "pop", "mom")
  df$parent_birth <- df$birth_year[match(df[, col], df$pid)]
  
  out <- 
    df %>% 
    select(birth_year, parent_birth) %>% 
    filter(between(birth_year, min(y_range), max(y_range))) %>% 
    dplyr::mutate(
      parent_age = birth_year - parent_birth 
      , parent_agegr_factor = cut(parent_age, age_breaks, age_labels, include.lowest = TRUE, 
                                  right = F)
      # , parent_agegr = as.character(parent_agegr_factor)
      , birth_year_factor = factor(birth_year, levels = y_range)
    ) %>%
    dplyr::count(birth_year = birth_year_factor, parent_agegr = parent_agegr_factor) %>% 
    tidyr::complete(birth_year, parent_agegr, fill = list(n = 0)) %>% 
    dplyr::filter(
      !is.na(parent_agegr)
      , birth_year %in% y_range 
    ) %>% 
    select(year = birth_year, agegr = parent_agegr, n) %>% 
    arrange(year, agegr)
  
  
  return(out)
  
}

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