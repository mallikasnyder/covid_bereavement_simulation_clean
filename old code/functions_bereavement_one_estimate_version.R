
#-----negation

'%ni%' <- Negate('%in%')

#-----na values
zna <- function(x){return(ifelse(x==0,NA,x))}


#----asYr

# a function to convert socsim months to calendar years
asYr <- function(FinalSimYear = FinalSimYear, endmo = endmo, x) {
  return(trunc(FinalSimYear - (endmo - x)/12)) #+1) #(removing this)
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
#Function for finding path

findPath <- function(folder, name) 
{paste(folder, name, "SimResults", "sims.Rsave", sep = "/")}


#----------findSimObject
findSimObject <- function(file){
  object <- load(file)
  return(get(object))
}


#---Kin Relationships

#-------add2opop2
add2opop2<-function(FinalSimYear = FinalSimYear, endmo = endmo, 
                    feb2020 = feb2020, age_br_months = age_br_months, age_labs = age_labs,
                    opop, omar){
  
  opop$spouse<-ifelse(omar[zna(opop$marid), "dstart"] <= feb2020 & 
                        omar[zna(opop$marid), "dend"] >= feb2020, 
                           ifelse(opop$fem,
                      (omar[zna(opop$marid),"hpid"]),
                      (omar[zna(opop$marid),"wpid"])), NA)
  
  #Find spouses, and accounting for re-marriages
  #omar2 <- as_tibble(omar) %>% dplyr::filter(dstart <= feb2020 & dend >=feb2020) 
  #opop_fem <- as_tibble(opop) %>% dplyr::filter(fem == 1)
  #opop_male <- as_tibble(opop) %>% dplyr::filter(fem == 0)
  #opop_fem$spouse <- omar2$hpid[match(opop_fem$pid, omar2$wpid)]
  #opop_male$spouse <- omar2$wpid[match(opop_male$pid, omar2$hpid)]
  
  #opop <- bind_rows(opop_fem, opop_male)
  
  opop <- opop %>%
    mutate(dod = ifelse(dod == 0, endmo, dod)) 
  
  ## Age in Feb2020
  opop$agefeb2020<- if_else(opop$dod >=feb2020, feb2020-opop$dob, NA_real_)
  #Grouping this by age group
  opop$agefeb2020_gr = as.character(cut(opop$agefeb2020, age_br_months, age_labs, include.lowest = T))
  
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
  opop$cohort<-asYr(FinalSimYear = FinalSimYear, endmo = endmo, x = opop$dob)
  #-#
  return(opop)
}


#-------getKin
#Finding kin for a vector of PIDS
getKin <- function(opop = opop, KidsOf = KidsOf, pid) {
#Creating a list to hold these various kin relationships
res<-list()

#--- The family tree above ego
##Great grandparents
#res$g0=lapply(pid,
#              function(pid){
#                as.vector(unlist(opop[opop$pid == pid ,
 #                                     c("MMM","MMF","MFM","MFF",
  #                                      "FMM","FMF","FFM","FFF")]))})

##Grandparents, great-uncles, great-aunts
#g1 = ko(KidsOf = KidsOf, p = res$g0)

#Grandparents
res$gparents=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                  c("MM","MF","FM","FF")]))})

#Great-uncles and aunts
#res$gunclesaunts = lapply(seq_along(g1), 
 #                         function(i) g1[[i]][g1[[i]] %ni% res$gparents[[i]]])

## Parents, uncles, and aunts
#g2=ko(KidsOf = KidsOf, p = res$gparents)

#Parents
res$parents=lapply(pid,
                   function(pid){
                     as.vector(unlist(opop[opop$pid == pid,
                                 c("mom","pop")]))})
#Uncles and Aunts
#res$unclesaunts = lapply(seq_along(g2), function(i) g2[[i]][g2[[i]] %ni% res$parents[[i]]])

#--The family tree at ego’s generation (g3)

##Cousins and Siblings
#g3 = ko(KidsOf = KidsOf, p = g2)

#Siblings
res$siblings = ko(KidsOf = KidsOf, p = res$parents)
#making sure to remove ego
res$siblings = lapply(seq_along(res$siblings), function(i) res$siblings[[i]][res$siblings[[i]] %ni% pid[[i]]])

#First cousins
#res$firstcousins = ko(KidsOf = KidsOf, p = res$unclesaunts)

#--The family tree below ego’s generation (g4 etc)

## Children, Nieces, and Nephews
#g4=ko(KidsOf = KidsOf, p = g3)

#Children
res$children = ko(KidsOf = KidsOf, p = pid)

#--In-laws

#Spouse
#res$spouse <- so(opop = opop, p = pid)

#Parents-in-law
#res$parentsofspouse=lapply(res$spouse,
  #                         function(x){
   #                          unlist(opop[opop$pid %in% x,
    #                                     c("mom","pop")])})

#Brothers and sister in laws = siblings of spouse/spouse of siblings
#Spousesofsiblings

#spouseofsiblings <- so(opop = opop, p = res$siblings)
#siblingsofspouse <- ko(KidsOf = KidsOf, p = res$parentsofspouse)
#siblingsofspouse <- lapply(seq_along(siblingsofspouse), 
  #                           function(i) siblingsofspouse[[i]][siblingsofspouse[[i]] %ni% res$spouse[[i]]])
#res$brothersisterinlaw <- mapply(c, spouseofsiblings, siblingsofspouse, SIMPLIFY=F)


#Aggregate kin counts
#All kin
res$all <- mapply(c, res$gparents, res$parents, res$children, res$siblings, SIMPLIFY = F)

#--Data cleaning
#Replacing NULLs and integer(0) with NA
#I'm not sure if this is necessary but I do it here in case it is helpful later.

for (i in 1:length(names(res))) {
  res[[i]][sapply(res[[i]], function(x) length(x)==0)] <- NA #removing NULLs and integer(0) with NA
}

return(res)
}

#----------ko function
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
  attr(kin2, "kin.dod") <- opop$dod[match(kin2, opop$pid)] #Thanks to Diego's excellent suggestion
  attr(kin2, "kin.agefeb2020") <-  opop$agefeb2020_gr[match(kin2, opop$pid)]
  attr(kin2, "kin.female") <- opop$fem[match(kin2, opop$pid)]
  attr(kin2, "still.alive.feb2020") <- if_else(attr(kin2, "kin.dod") > feb2020, TRUE, FALSE)
  attr(kin2, "still.alive.postcovid2020") <- if_else(attr(kin2, "kin.dod") > postcovid2020, TRUE, FALSE)
  
  return(kin2)
}



#------------getDeathRates
getDeathRates <- function(df, sim.id, country, scenario,
                          get_death_rate = T, 
                          get_asmr = T, age_br_months, 
                          age_labs, FinalSimYear, 
                          endmo, feb2020, postcovid2020){
  
  # From socsim opop file
  # Do all estimates in months!
  # Feb 2020 is feb2020 = 3241
  # feb2020 should exist already 
  
  # Does not save results to disk
  
  # This estimates the mortality rate for the entire year
  # month_range <- c(feb2020 - 1, feb2020 + 10)
  # This estimates the mortality rate for Feb - to the end of the pandemic
  # assumed to be August 2020
  month_range_low <-  feb2020 #feb2020 - 1
  month_range_high <- postcovid2020    #feb2020 + 6
  
  mid_period_month <- trunc(month_range_low + diff(c(month_range_low, month_range_high))/2)
  
  # A dataframe of people alive at start of period
  alive_at_start <- 
    df %>% 
    mutate(
      dod2 = ifelse(dod == 0, endmo, dod)
      , age_death_months = dod2-dob #ifelse(dod == 0,NA,dod-dob)
      , age_mid_period_months = mid_period_month-dob #ifelse(dod == 0,NA,mid_period_month-dob)
    ) %>% 
    dplyr::select(dob, dod2, fem, age_death_months, age_mid_period_months) %>% 
    mutate(
      age_death_gr = cut(age_death_months, age_br_months, age_labs, include.lowest = T)
      , age_mid_period_gr = cut(age_mid_period_months, age_br_months, age_labs, include.lowest = T)
    ) %>% 
    dplyr::filter(dob <= month_range_low & dod2 > month_range_low) #removed: dod2 = month_range_low
  
  # DEATH RATE
  if(get_death_rate){
    
    deaths_vector <- alive_at_start$dod2
    
    mid_period_pop <- sum(deaths_vector >= mid_period_month)
    
    # This is already a vector born before start of period
    # and who died beore of period, so just need
    # to filter to keep those that died in the given interval
    deaths_period <- sum(deaths_vector <= month_range_high)
    
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
  
    
    #First object:ratios from kin
    #Size of family (nuclear and extended)
    #Age ratio of relatives
    #Sex ratio of relatives
    kin_ratio <- bind_rows(lapply(all_cat, function(x) {
      #All egos
      ego.names <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                             attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2])]))
    
      #Number of kin alive in Feb 2020
      #All kin
      count_table <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), 
                            levels = ego.names))
      
      #Female kin
      kin_female <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE & 
                                                    attr(kin2, "kin.female") == 1 & 
                                                    attr(kin2, "kin.agefeb2020") %in% c("15-29", "30-44", "45-64", "65+"))]), 
                                                levels = ego.names))
      #Male kin
      kin_male <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE & 
                                                    attr(kin2, "kin.female") == 0 & 
                                                  attr(kin2, "kin.agefeb2020") %in% c("15-29", "30-44", "45-64", "65+"))]), 
                                 levels = ego.names))
      #Kin above 65
      kin_65plus <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE & 
                                                    attr(kin2, "kin.agefeb2020") %in% c("65+"))]), 
                                 levels = ego.names))
      
      #Kin below 65
      kin_below65 <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE & 
                                                     attr(kin2, "kin.agefeb2020") %in% c("15-29", "30-44", "45-64"))]), 
                                  levels = ego.names))
      
      output <- list(count_all = mean(count_table, na.rm = T), sd_all = sd(count_table, na.rm = T), 
                     count_female = mean(kin_female, na.rm = T), count_male = mean(kin_male, na.rm = T),
                     count_65plus = mean(kin_65plus, na.rm = T), count_below65 = mean(kin_below65, na.rm = T),
                     n_egos = length(count_table),
                     category = x, kintype = kin.type, sim.id = sim.id, country = country, scenario = scenario)
      
      return(output)
    }))
    
  
    #Second object: the burden of bereavement
    #Find the average number of kin before and after for those who survive: it will return a tibble
    kin_bb <- bind_rows(lapply(all_cat, function(x) {
      
    #Find names of all survivors
    ego.names.bb <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                              attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                              attr(kin2, "ego.dod") >= postcovid2020)]))
    
    #Find kin counts for all individuals
    num_pre_all <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), 
                            levels = ego.names.bb))
    
    num_post_all <- table(factor(names(kin2[which(attr(kin2, "still.alive.postcovid2020") == TRUE)]), 
                             levels = ego.names.bb))
    
    
    #Number of egos that actually have kin of the type in 2020
    ego.withkin <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                             attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                             attr(kin2, "still.alive.feb2020") == TRUE &
                                             attr(kin2, "ego.dod") >= postcovid2020)]))
    
    #Kin counts for these egos
    num_pre_with <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), 
                                levels = ego.withkin))
    
    num_post_with <- table(factor(names(kin2[which(attr(kin2, "still.alive.postcovid2020") == TRUE)]), 
                                 levels = ego.withkin))
    
    #Number of egos that lose kin
    ego.losekin <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                             attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                             attr(kin2, "still.alive.feb2020") == TRUE &
                                             attr(kin2, "still.alive.postcovid2020") == FALSE &
                                             attr(kin2, "ego.dod") >= postcovid2020)]))
    
    
    #Kin counts for these egos
    num_pre_lose <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE & 
                                                    attr(kin2, "still.alive.postcovid2020") == FALSE)]), 
                                 levels = ego.withkin))
    
    num_post_with <- table(factor(names(kin2[which(attr(kin2, "still.alive.postcovid2020") == TRUE)]), 
                                  levels = ego.withkin))
  
    output <- list(n_total = length(num_pre_all), 
                   mean_pre_all = mean(num_pre_all, na.rm = T), sd_pre_all = sd(num_pre_all, na.rm = T), 
                   mean_post_all = mean(num_post_all, na.rm = T), sd_post_all = sd(num_post_all, na.rm = T),
                   n_withkin = length(ego.withkin),
                   mean_pre_with = mean(num_pre_with, na.rm = T), sd_pre_with = sd(num_pre_with, na.rm = T),
                   mean_post_with = mean(num_post_with, na.rm = T), sd_post_with = sd(num_post_with, na.rm = T),
                   n_losekin = length(ego.losekin),
                   category = x, kintype = kin.type, sim.id = sim.id, country = country, scenario = scenario)
    return(output)
    
  }))
  
    kin_both <- list(kin_ratio = kin_ratio, kin_bb = kin_bb)
    
    return(kin_both)
}

#-------Estimates function
getEstimates <- function(sim.id, country, scenario, USER,
                            feb2020 = feb2020, postcovid2020 = postcovid2020) {
  #First level needs
  folder <- paste0("/90days/", USER, "/", scenario, "RESULTS/", "SocCovid-mallikasnyder/", country, ":Medium.sup")
  
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
  
  #Filtering to only individuals born before or in February 2020: this is month 3241
  #I found that by taking the second unique value of dates of birth for individuals in the 2020 birth cohort
  opop <- opop[opop$dob <= feb2020,] #do we want to filter on dod (& opop$dod > feb2020)? Put minimum limit on dob as well
  
  #Filtering cohorts pre-1800: we may want to reconsider this
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
  
  #Bind into tibbles
  flat_data <- do.call(c, temp_data)
  data_kin <- bind_rows(flat_data[which(names(flat_data) %in% "kin_ratio")])
  data_bb <- bind_rows(flat_data[which(names(flat_data) %in% "kin_bb")])
  
  #Add death_rates
  data <- list(kin_ratio = data_kin, kin_bb = data_bb, 
               death_rates = death_rates)
  
  #Save intermediate output
  save(data, file = paste0("~/covid_simulation/Data/estimates/", "data", sim.id, "_", country, "_", scenario, ".RData"))
  
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
    
    #length(n_pre) = length(n_post)
    output <- list(mean_pre = mean(num_pre, na.rm = T), sd_pre = sd(num_pre, na.rm = T), n_survivors = length(num_pre), 
                   mean_post = mean(num_post, na.rm = T), sd_post = sd(num_post, na.rm = T),
                   category = x, kintype = kin.type, sim.id = sim.id, country = country, scenario = scenario)
    return(output)
    
  }))
  
  
  return(kin_female)
}