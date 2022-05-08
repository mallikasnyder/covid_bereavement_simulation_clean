#This script finds counts of female kin by cohort for women in Sweden

setwd("~/covid_bereavement_simulation_clean/")

#Source functions and packages
source("~/covid_bereavement_simulation_clean/Kin_death/load_functions.R")
source('~/covid_bereavement_simulation_clean/Kin_death/functions_bereavement.R')

#Directory stems
dir_stem <- c("covid", "other")

#Keep countries (for partial results)
country_partial <- c("Sweden")

#Find details for all simulations
list_countries <- lapply(dir_stem, function(y) {
  scenario <- y
  filepath <- paste0("~/90days/", y, "RESULTS/", "SocCovid-mallikasnyder/")
  countries <- grep(list.files(filepath), pattern = paste0(country_partial, collapse = "|"), invert = F, value = T)
  countryvec <- lapply(countries, function(x) {
    simnames <- grep(pattern = ".opop", 
                     list.files(paste0(filepath, x, "/")), 
                     invert = T, value = T)
    scenarionames = rep(scenario, length(simnames))
    countrynames = rep(gsub(":Medium.sup", "", x), length(simnames))
    
    output = list(sim = simnames, scenario = scenarionames, country = countrynames)
    return(output)
  })
}
)

#Flatten this list
flat_countries <- do.call(c, do.call(c, list_countries))

#Create vectors for all sims, all countries, and all scenarios
allsims <- unlist(flat_countries[which(names(flat_countries) == "sim")], use.names = F)
allcountries <- unlist(flat_countries[which(names(flat_countries) == "country")], use.names = F) 
allscenarios <- unlist(flat_countries[which(names(flat_countries) == "scenario")], use.names = F) 

#Set up input values
feb2020 <- 3242
postcovid2020 <- 3242+6-2+1

#Function for doing this

getCountsFemale <- function(sim.id, country, scenario, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020) {
  #First level needs
  folder <- paste0("~/90days/", scenario, "RESULTS/", 
                   "SocCovid-mallikasnyder/", country, ":Medium.sup")
  
  print(paste0("Starting:", sim.id))
  
  #Remove the objects
  #Now load the data
  sims <- findSimObject(findPath(folder = folder, name = sim.id))
  #Same as load(file = findPath(sim.id))
  
  # Function parameters for asYr()
  FinalSimYear <- 2035 #Are these correct? #The first month is 0, the last month is 3240
  #It makes sense we have one extra month in 2035, since our first month (month 1) is actually a February
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
  
  #Filtering to only individuals alive in February 2020: this is month 3241
  #I found that by taking the second unique value of dates of birth for individuals in the 2020 birth cohort
  opop <- opop[opop$dob <= feb2020,] #do we want to filter on dod (& opop$dod > feb2020)? Put minimum limit on dob as well
  
  #Filtering cohorts pre-1800: we may want to reconsider this
  opop <- opop[opop$cohort >= 1800,]
  
  #Keeping only women in this dataset
  opop <- opop[which(opop$fem == 1),]
  
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
  kin <- getKinFemale(opop = opop, KidsOf = KidsOf, pid = pid)
  
  #All categories: age and sex
  all_cat <- unique(paste0(ego.agefeb2020, "f", ego.female))
  
  #Run the analysis code
  data <- lapply(1:length(kin), function(x) getKinCountsFemale(all_cat = all_cat, 
                                                                 kin = kin[[x]], kin.type = names(kin[x]), 
                                                                 pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                                                                 ego.female = ego.female, opop = opop, 
                                                                 feb2020 = feb2020, postcovid2020 = postcovid2020, 
                                                                 sim.id = sim.id, country = country, scenario = scenario))
  
  #Bind into tibbles
  countdata <- bind_rows(data)
  
  #Save intermediate output
  save(countdata, file = paste0("~/covid_bereavement_simulation_clean/Data/Sweden_female_data/female_kin_data_", sim.id, ".RData"))
  
  print(paste0("Estimates generated:", sim.id))
  
}


#Run this
final <- mclapply(1:length(allsims), function(x) 
  try(getCountsFemale(sim.id = allsims[x], country = allcountries[x], 
                      scenario = allscenarios[x], 
         feb2020 = feb2020, postcovid2020 = postcovid2020)), mc.cores = 10)
