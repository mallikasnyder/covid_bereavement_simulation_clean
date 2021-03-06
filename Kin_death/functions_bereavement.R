
#This script contains the functions used for analyzing the scripts
#It draws on code written by Carl Mason and Diego Alburez-Gutierrez

#-----negation

'%ni%' <- Negate('%in%')

#-----na values
zna <- function(x){return(ifelse(x==0,NA,x))}


#----asYr

# a function to convert socsim months to calendar years
asYr <- function(FinalSimYear = FinalSimYear, endmo = endmo, x) {
  return(trunc(FinalSimYear - (endmo - x)/12))
}


#---------Library2
# adapted from pacman package
library2 <- function (package1, ...) {
  packages <- c(package1, ...)
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {
      suppressPackageStartupMessages( do.call(library, list(package)) )
      print(paste("library2:",package, "loaded."))
    }
    else {
      tryCatch({
        install.packages(package)
        suppressPackageStartupMessages( do.call(library, list(package)) )
      }, error = function(e) {
      })
    }
  }
}


#----------findPath
#Function for finding file path

findPath <- function(folder, name) 
{paste(folder, name, "SimResults", "sims.Rsave", sep = "/")}


#----------findSimObject
#Function for finding simulation object
findSimObject <- function(file){
  object <- load(file)
  return(get(object))
}


#---Kin Relationships

#-------add2opop2 (adapted from code written by Carl Mason)
add2opop2<-function(FinalSimYear = FinalSimYear, endmo = endmo, 
                    feb2020 = feb2020, age_br_months = age_br_months, age_labs = age_labs,
                    opop, omar){
  
  #We do not consider spouses
  opop$spouse<-ifelse(omar[zna(opop$marid), "dstart"] <= feb2020 & 
                        omar[zna(opop$marid), "dend"] >= feb2020, 
                           ifelse(opop$fem,
                      (omar[zna(opop$marid),"hpid"]),
                      (omar[zna(opop$marid),"wpid"])), NA)

  #Individuals who do not die by the end of the simulation are originally assigned dod=0
  #We replace it with the endmo here
  opop <- opop %>%
    mutate(dod = ifelse(dod == 0, endmo, dod)) 
  
  ## Age in Feb2020
  opop$agefeb2020<- if_else(opop$dod >=feb2020, feb2020-opop$dob, NA_real_)
  #Grouping this by age group
  opop$agefeb2020_gr = as.character(cut(opop$agefeb2020, age_br_months, age_labs, include.lowest = T))
  
  #Identifying ascendant kin
  opop$FM<-opop$mom[match(opop$pop, opop$pid)]
  opop$MM<-opop$mom[match(opop$mom, opop$pid)]
  opop$MF<-opop$pop[match(opop$mom, opop$pid)]
  opop$FF<-opop$pop[match(opop$pop, opop$pid)]
  
  opop$FFM<-opop$FM[match(opop$pop, opop$pid)]
  opop$FMM<-opop$MM[match(opop$pop, opop$pid)]
  opop$FMF<-opop$MF[match(opop$pop, opop$pid)]
  opop$FFF<-opop$FF[match(opop$pop, opop$pid)]
  
  opop$MFM<-opop$FM[match(opop$mom, opop$pid)]
  opop$MMM<-opop$MM[match(opop$mom, opop$pid)]
  opop$MMF<-opop$MF[match(opop$mom, opop$pid)]
  opop$MFF<-opop$FF[match(opop$mom, opop$pid)]
  
  #Birth cohort
  opop$cohort<-asYr(FinalSimYear = FinalSimYear, endmo = endmo, x = opop$dob)
  #-#
  return(opop)
}


#-------getKin
#Finding kin for a vector of PIDS
getKin <- function(opop = opop, KidsOf = KidsOf, pid) {
#Creating a list to hold these various kin relationships
res<-list()

#Grandparents
res$gparents=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                  c("MM","MF","FM","FF")]))})

#Parents
res$parents=lapply(pid,
                   function(pid){
                     as.vector(unlist(opop[opop$pid == pid,
                                 c("mom","pop")]))})

#Siblings
res$siblings = ko(KidsOf = KidsOf, p = res$parents)
#Making sure to remove ego
res$siblings = lapply(seq_along(res$siblings), function(i) res$siblings[[i]][res$siblings[[i]] %ni% pid[[i]]])

#Children
res$children = ko(KidsOf = KidsOf, p = pid)

#--Data cleaning
#Replacing NULLs and integer(0) with NA
for (i in 1:length(names(res))) {
  res[[i]][sapply(res[[i]], function(x) length(x)==0)] <- NA #removing NULLs and integer(0) with NA
}

return(res)
}

#----------ko function (written by Carl Mason)
#Kids of PID
ko <- function(KidsOf = KidsOf, p){
  lapply(p,function(x){unique(as.vector(unlist(KidsOf[x])))})
}

#-----------so function
# Spouse of PID
so <- function(opop = opop, p){
  lapply(p,function(p){as.vector(unlist(opop[opop$pid %in% p,
                                             c("spouse")]))})
}

#-----------getVectorizedForm
#Vectorizing the kin relationships
getVectorizedForm <- function(pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                              ego.female = ego.female, opop = opop, 
                              feb2020 = feb2020, postcovid2020 = postcovid2020,
                              object) {
  kin2 <- unlist(object, use.names = F)
  names(kin2) <- rep(pid, unlist(lapply(object, length))) #names are the person IDs they are associated with
  #Ego attributes
  attr(kin2, "ego.agefeb2020") <- rep(ego.agefeb2020, unlist(lapply(object, length))) #adding an attribute for PID age of death
  attr(kin2, "ego.dod") <- rep(ego.dod, unlist(lapply(object, length))) #adding an attribute for PID age of death
  attr(kin2, "ego.female") <- rep(ego.female, unlist(lapply(object, length))) #adding an attribute for PID sex
  
  #Kin attributes
  attr(kin2, "kin.dod") <- opop$dod[match(kin2, opop$pid)]
  attr(kin2, "kin.agefeb2020") <-  opop$agefeb2020_gr[match(kin2, opop$pid)]
  attr(kin2, "kin.female") <- opop$fem[match(kin2, opop$pid)]
  attr(kin2, "still.alive.feb2020") <- if_else(attr(kin2, "kin.dod") > feb2020, TRUE, FALSE)
  attr(kin2, "still.alive.postcovid2020") <- if_else(attr(kin2, "kin.dod") > postcovid2020, TRUE, FALSE)
  
  return(kin2)
}



#------------getDeathRates
#This function is used for the comparison between 
#STMF input and simulation death rates in the appendix
#Not all data outputs are used
getDeathRates <- function(df, sim.id, country, scenario,
                          get_death_rate = T, 
                          get_asmr = T, age_br_months, 
                          age_labs, FinalSimYear, 
                          endmo, feb2020, postcovid2020){
  
  # From socsim opop file
  # Do all estimates in months!
  # feb2020 should exist already 
  
  # This estimates the mortality rate between March 2020 and June 2021
  month_range_low <-  feb2020
  month_range_high <- postcovid2020
  
  mid_period_month <- trunc(month_range_low + diff(c(month_range_low, month_range_high))/2)
  
  # A dataframe of people alive at start of period
  alive_at_start <- 
    df %>% 
    mutate(
      dod2 = ifelse(dod == 0, endmo, dod)
      , age_death_months = dod2-dob
      , age_mid_period_months = mid_period_month-dob
    ) %>% 
    dplyr::select(dob, dod2, fem, age_death_months, age_mid_period_months) %>% 
    mutate(
      age_death_gr = cut(age_death_months, age_br_months, age_labs, include.lowest = T)
      , age_mid_period_gr = cut(age_mid_period_months, age_br_months, age_labs, include.lowest = T)
    ) %>% 
    dplyr::filter(dob <= month_range_low & dod2 > month_range_low)
  
  # DEATH RATE
  if(get_death_rate){
    
    deaths_vector <- alive_at_start$dod2
    
    mid_period_pop <- sum(deaths_vector >= mid_period_month)
    
    # This is already a vector born before start of period
    # and who died beore of period, so just need
    # to filter to keep those that died in the given interval
    deaths_period <- sum(deaths_vector < month_range_high)
    
    death_rate <- deaths_period/mid_period_pop * 1000
  
  }
  
  # Get Age-specific mortality rate for given age groups
  if(get_asmr){
    
    # Numerator - death counts by age and sex
    
    numerator <- 
      alive_at_start %>% 
      # keep only those that died before end of period
      dplyr::filter(dod2 < month_range_high) %>%  #removed =
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
        , sim.id = sim.id
        , age = as.character(age)
        , fem = as.character(fem)
      ) 
    
  }
  
  death_rates <- asmr %>%
    add_row(fem = "all_sum", age = "all_sum", 
            n_num = sum(asmr$n_num), n_den = sum(asmr$n_den), value = 1000*(n_num/n_den),
            sim.id = sim.id) %>%
    add_row(value = death_rate, fem = "all_mid", age = "all_mid", 
            n_num = deaths_period, n_den = mid_period_pop, sim.id = sim.id) %>%
    mutate(country = country, scenario = scenario)
  
  return(death_rates)
  
  print("Death rates generated")
}

#-------Analysis functions
#-------getKinCounts
getKinCounts <- function(all_cat = all_cat, kin, kin.type, pid = pid, 
                            ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                            ego.female = ego.female, opop = opop, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020, 
                            sim.id = sim.id, country = country, scenario = scenario) {
  
    #Vectorize kin counts
    kin2 <- getVectorizedForm(pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                            ego.female = ego.female, opop = opop, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020,
                            object = kin)
  
  
    #Main object: the burden of bereavement
    #Find the average number of kin before and after for those who survive: it will return a tibble
    kin_bb <- bind_rows(lapply(all_cat, function(x) {
      
    #Find names of all survivors
    ego.names.bb <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                              attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                              attr(kin2, "ego.dod") >= postcovid2020)]))
    #Used for finding number of survivors
    num_pre_all <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), 
                                levels = ego.names.bb))
    
  
    #Number of egos that actually have kin of the type in 2020
    ego.withkin <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                             attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                             attr(kin2, "still.alive.feb2020") == TRUE &
                                             attr(kin2, "ego.dod") >= postcovid2020)]))
    
    #Monthly estimates of kin loss
    kinloss <- bind_rows(lapply((feb2020 + 1): postcovid2020, function(z) {
    
    #Number of egos that lose kin
    ego.losekin <- length(unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                             attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                             attr(kin2, "still.alive.feb2020") == TRUE &
                                             attr(kin2, "kin.dod") == z &
                                             attr(kin2, "ego.dod") >= postcovid2020)])))
    
    losekin.tibble <- tibble(n_losekin = ego.losekin, month = z)
    return(losekin.tibble)
    }))
    
    n_months <- length((feb2020 + 1): postcovid2020)
    
    other_output <- tibble(n_total = rep(length(num_pre_all), n_months), 
                   n_withkin = rep(length(ego.withkin), n_months),
                   category = rep(x, n_months), kintype = rep(kin.type, n_months), 
                   sim.id = rep(sim.id, n_months), 
                   country = rep(country, n_months), scenario = rep(scenario, n_months))
    
    output <- bind_cols(other_output, kinloss)
    return(output)
    
  }))
  
    
    return(kin_bb)
}

#-------Estimates function
getEstimates <- function(sim.id, country, scenario, USER,
                            feb2020 = feb2020, postcovid2020 = postcovid2020) {
  #Identifying folder
  folder <- paste0("/90days/", USER, "/", scenario, "RESULTS/", "SocCovid-mallikasnyder/", country, ":Medium.sup")
  
  print(paste0("Starting:", sim.id))
  
  #Remove the objects
  #Now load the data
  sims <- findSimObject(findPath(folder = folder, name = sim.id))
  
  # Function parameters for asYr()
  FinalSimYear <- 2035
  endmo <- 3421
  
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
  
  #Filtering to only individuals born before or in February 2020
 opop <- opop[opop$dob <= feb2020,] 
  
  #Filtering cohorts pre-1800
  opop <- opop[opop$cohort >= 1800,]
  
  #Calculate death rates
  death_rates <- getDeathRates(
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
  )
  
  #Person ID vector for all egos
  #Filtering so we get kin for only those individuals who survive the Covid period or die during it
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
  kin <- getKin(opop = opop, KidsOf = KidsOf, pid = pid)
  
  #All categories: age and sex
  all_cat <- unique(paste0(ego.agefeb2020, "f", ego.female))
  
  #Get Kin Data
  temp_data <- lapply(1:length(kin), function(x) getKinCounts(all_cat = all_cat, 
                                                            kin = kin[[x]], kin.type = names(kin[x]), 
                                                            pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                                                            ego.female = ego.female, opop = opop, 
                                                            feb2020 = feb2020, postcovid2020 = postcovid2020, 
                                                            sim.id = sim.id, country = country, scenario = scenario))
  
  #Bind into tibble
  data_bb <- bind_rows(temp_data)
  
  #Add death_rates
  data <- list(kin_bb = data_bb, 
               death_rates = death_rates)
  
  #Save intermediate output
  save(data, file = paste0("~/covid_bereavement_simulation_clean/Data/estimates/", "data", sim.id, "_", country, "_", scenario, ".RData"))
  
  print(paste0("Estimates generated:", sim.id))
  
}

#-----Functions for female kin


#Version that only finds female kin
#Great-grandmothers, grandmothers, mothers, daughters
getKinFemale <- function(opop = opop, KidsOf = KidsOf, pid) {
  #Creating a list to hold these various kin relationships
  res<-list()
  
  #--- The family tree above ego
  
  #Great-grandparents
  res$ggmothers=lapply(pid,
            function(pid){
              as.vector(unlist(opop[opop$pid == pid ,
                                    c("MMM")]))})
  
  #Grandmothers 
  res$gmothers=lapply(pid,
                      function(pid){
                        as.vector(unlist(opop[opop$pid == pid,
                                              c("MM")]))})
  #Mothers
  res$mother=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                            c("mom")]))})
  
  #Daughters
  res$daughters = ko(KidsOf = KidsOf, p = pid)
  
  #--Data cleaning
  #Replacing NULLs and integer(0) with NA
  #I'm not sure if this is necessary but I do it here in case it is helpful later.
  
  for (i in 1:length(names(res))) {
    res[[i]][sapply(res[[i]], function(x) length(x)==0)] <- NA #removing NULLs and integer(0) with NA
    #res[[i]] <- sapply(res[[i]], function(x) unname(x)) #Removing names for space storage
  }
  
  return(res)
}

#Kin counts
getKinCountsFemale <- function(all_cat = all_cat, kin, kin.type, pid = pid, 
                               ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                               ego.female = ego.female, opop = opop, 
                               feb2020 = feb2020, postcovid2020 = postcovid2020, 
                               sim.id = sim.id, country = country, scenario = scenario) {
  
  #Vectorize kin counts
  kin2 <- getVectorizedForm(pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                            ego.female = ego.female, opop = opop, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020,
                            object = kin)
  
  
    #Find the average number of kin before and after for those who survive: it will return a tibble
    kin_female <- bind_rows(lapply(all_cat, function(x) {
    #Find names of survivors
    ego.names.bb <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                              attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                              attr(kin2, "ego.dod") >= postcovid2020)]))
    
    #Find kin counts
    num_pre <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), 
                            levels = ego.names.bb))
    
    num_post <- table(factor(names(kin2[which(attr(kin2, "still.alive.postcovid2020") == TRUE)]), 
                             levels = ego.names.bb))
    
    #Bind into a list
    output <- list(mean_pre = mean(num_pre, na.rm = T), sd_pre = sd(num_pre, na.rm = T), n_survivors = length(num_pre), 
                   mean_post = mean(num_post, na.rm = T), sd_post = sd(num_post, na.rm = T),
                   category = x, kintype = kin.type, sim.id = sim.id, country = country, scenario = scenario)
    return(output)
    
  }))
  
  
  return(kin_female)
}

#------Function to only get death rates (for quick checking)
getDeathRatesOnly <- function(sim.id, country, scenario, USER,
                         feb2020 = feb2020, postcovid2020 = postcovid2020) {
  #First level needs
  folder <- paste0("/90days/", USER, "/", scenario, "RESULTS/", "SocCovid-mallikasnyder/", country, ":Medium.sup")
  
  print(paste0("Starting:", sim.id))
  
  #Remove the objects
  #Now load the data
  sims <- findSimObject(findPath(folder = folder, name = sim.id))
  #Same as load(file = findPath(sim.id))
  
  # Function parameters for asYr()
  FinalSimYear <- 2035
  endmo <- 3421
  
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
  
  #Filtering to only individuals born before or in February 2020
opop <- opop[opop$dob <= feb2020,]
  
  #Filtering cohorts pre-1800
  opop <- opop[opop$cohort >= 1800,]
  
  #Calculate death rates
  death_rates <- getDeathRates(
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
  )
  
  #Add death_rates
  data_death_rates <- list(death_rates = death_rates)
  
  #Save intermediate output
  save(data_death_rates, file = paste0("~/covid_bereavement_simulation_clean/Data/death_rates/", "data", sim.id, "_", country, "_", scenario, ".RData"))
  
  print(paste0("Estimates generated:", sim.id))
  
}