getEstimatesPreprint <- function(sim.id, country, scenario, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020) {
  #First level needs
  folder <- paste0("~/90days/", scenario, "RESULTS/", "SocCovid-mallikasnyder/", country, ":Medium.sup")
  
  print(paste0("Starting:", sim.id))
  
  #Remove the objects
  #Now load the data
  sims <- findSimObject(findPath(folder = folder, name = sim.id))
  #Same as load(file = findPath(sim.id))
  
  # Function parameters for asYr()
  FinalSimYear <- 2035 #Are these correct? #The first month is 0, the last month is 3240
  #It makes sense we have one extra month in 2035, since our first month (month 1) is actually a February
  endmo <- 3420
  
  #Parameters for the add2opop2 and death rate functions
  age_br_months <- c(0, 14, 29, 44, 64, Inf)*12
  age_labs <- c("0-14", "15-29", "30-44", "45-64", "65+")
  
  
  #Adding opop to omar
  opop <- add2opop2(FinalSimYear = FinalSimYear, endmo = endmo, 
                    feb2020 = feb2020, age_br_months = age_br_months, age_labs = age_labs,
                    opop = sims$opop, omar = sims$omar)
  
  #Filtering out columns we don't need
  opop <- opop[, -which(names(opop) %in% 
                          c("group", "nev", "nesibm", "nesibp", 
                            "lborn", "marid", "mstat", "fmult"))]
  
  #Filtering to only individuals alive in February 2020: this is month 3241
  #I found that by taking the second unique value of dates of birth for individuals in the 2020 birth cohort
  opop <- opop[opop$dob <= feb2020,] #do we want to filter on dod (& opop$dod > feb2020)? Put minimum limit on dob as well
  
  #Filtering cohorts pre-1800: we may want to reconsider this
  opop <- opop[opop$cohort >= 1800,]
  
  #Calculate death rates
  death_rates <- save_mortality_rates_first_wave_to_disk(
    df = opop
    , sim.id = sim.id
    , country = country
    , scenario = scenario
    , get_death_rate = T
    , get_asmr = T
    , age_br_months = age_br_months
    , age_labs = age_labs
    , FinalSimYear = FinalSimYear
    , endmo = endmo
    , feb2020 = feb2020
    , postcovid2020 = postcovid2020
    , path_out = "~/covid_simulation/Data/death_rates/"
  )
  
  
  #Calculate dependency ratios
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
  
  #Person ID vector for all egos
  #Filtering so we get kin for only those individuals who either survive the Covid period or who die during it
  pid <- opop$pid[opop$dod > feb2020]
  
  #Vector with age intervals of egos
  ego.agefeb2020 <- opop$agefeb2020_gr[opop$pid %in% pid]
  #Vector with months of death of egos
  ego.dod <-opop$dod[opop$pid %in% pid]
  #Vector with sex of egos
  ego.female <- opop$fem[opop$pid %in% pid]
  
  #Preparing to find kin relationships
  # Function for finding children
  kidsOf<-with(opop,{c(
    tapply(pid,mom,c),
    tapply(pid,pop,c)
  )})
  
  #Using indexes saves space
  #We have to run both of these since there are two 0s
  kidsOf["0"]<-NULL;  kidsOf["0"]<-NULL
  KidsOf<-list()
  KidsOf[as.numeric(names(kidsOf))]<-kidsOf
  
  #Find kin of these PIDs
  kin <- getKinTogether(opop = opop, KidsOf = KidsOf, pid = pid)
  
  #All categories: age and sex
  all_cat <- unique(paste0(ego.agefeb2020, "f", ego.female))
  
  #Run the analysis code
  temp_data <- lapply(1:length(kin), function(x) getKinCountsPreprint(all_cat = all_cat, 
                                                                 kin = kin[[x]], kin.type = names(kin[x]), 
                                                                 pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                                                                 ego.female = ego.female, opop = opop, 
                                                                 feb2020 = feb2020, postcovid2020 = postcovid2020, 
                                                                 sim.id = sim.id, country = country, scenario = scenario))
  
  #Bind into tibbles
  flat_data <- do.call(c, temp_data)
  data_bm <- bind_rows(flat_data[which(names(flat_data) %in% "kin_bm")])
  data_bb <- bind_rows(flat_data[which(names(flat_data) %in% "kin_bb")])
  
  #Add death_rates
  data <- list(kin_bm = data_bm, kin_bb = data_bb, 
               death_rates = death_rates)
  
  #Save intermediate output
  save(data, file = paste0("~/covid_simulation/Data/sim_results_paa/data", sim.id, ".RData"))
  
  print(paste0("Estimates generated:", sim.id))
  
}


getKinCountsPreprint <- function(all_cat = all_cat, kin, kin.type, pid = pid, 
                            ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                            ego.female = ego.female, opop = opop, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020, 
                            sim.id = sim.id, country = country, scenario = scenario) {
  
  #Vectorize kin counts
  kin2 <- getVectorizedForm(pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                            ego.female = ego.female, opop = opop, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020,
                            object = kin)
  
  #Now we want to run two different processes on the data
  #The first is to calculate the bereavement multiplier: average number of kin for individuals who die during the Covid period
  
  kin_bm <- bind_rows(lapply(all_cat, function(x) {
    #All egos who died
    ego.names <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                           attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                           data.table::between(attr(kin2, "ego.dod"), 
                                                               feb2020, postcovid2020, incbounds = F))]))
    
    #Kin for those who died
    table <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), 
                          levels = ego.names))
    
    output <- list(mean = mean(table, na.rm = T), sd = sd(table, na.rm = T), n_deaths = length(table),
                   category = x, kintype = kin.type, sim.id = sim.id, country = country, scenario = scenario)
    
    return(output)
  }))
  
  #The second is to find the average number of kin before and after for those who survive: it will return a tibble
  kin_bb <- bind_rows(lapply(all_cat, function(x) {
    #Find names of survivors
    ego.names.bb <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                              attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                              attr(kin2, "ego.dod") >= postcovid2020)]))
    
    #Find kin counts
    num_pre <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), 
                            levels = ego.names.bb))
    
    num_post <- table(factor(names(kin2[which(attr(kin2, "still.alive.postcovid2020") == TRUE)]), 
                             levels = ego.names.bb))
    
    #length(n_pre) = length(n_post)
    output <- list(mean_pre = mean(num_pre, na.rm = T), sd_pre = sd(num_pre, na.rm = T), n_survivors = length(num_pre), 
                   mean_post = mean(num_post, na.rm = T), sd_post = sd(num_post, na.rm = T),
                   category = x, kintype = kin.type, sim.id = sim.id, country = country, scenario = scenario)
    return(output)
    
  }))
  
  kin_both <- list(kin_bm = kin_bm, kin_bb = kin_bb)
  
  return(kin_both)
}


#Taking a census

getCensus <- function(df = opop, y = 2018:2020 , return_ids = T, group_by_sex = F, group_by_age_sex = F) {
  
  df$census <- y
  
  df$death_temp <- asYr(df$dod)
  
  year_df <- 
    df %>% 
    dplyr::filter(asYr(dob) <= y & death_temp >= y)
  
  if(return_ids) {
    out <- year_df$pid
  } else {
    if(group_by_sex) {
      out <- year_df %>% 
        dplyr::count(fem, census)
    } else if(group_by_age_sex) {
      out <- year_df %>% 
        dplyr::mutate(
          age_at_census = census - asYr(dob)
        ) %>% 
        dplyr::count(fem, age_at_census, census) %>% 
        tidyr::complete(fem, age_at_census, census, fill = list(n = 0))
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



