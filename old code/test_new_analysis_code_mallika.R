setwd("~/covid_simulation/")

#Source functions and packages
source("~/covid_simulation/Kin_death/01_load_functions.R")
source('~/covid_simulation/Kin_death/functions_bereavement.R')

#Directory stems
dir_stem <- c("covid", "other")

#Keep countries (for partial results)
country_partial <- c("United_States_of_America", "United_Kingdom", "Germany", "France", "Sweden")

#Find details for all simulations
list_countries <- lapply(dir_stem, function(y) {
  scenario <- y
  filepath <- paste0("~/90days/", y, "RESULTS/", "SocCovid-mallikasnyder/")
  countries <- grep(list.files(filepath), pattern = paste0(country_partial, collapse = "|"), value = T)
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

save(flat_countries, file = "~/covid_simulation/Data/countrylist_partial.RData")

#Create vectors for all sims, all countries, and all scenarios
allsims <- unlist(flat_countries[which(names(flat_countries) == "sim")], use.names = F)
allcountries <- unlist(flat_countries[which(names(flat_countries) == "country")], use.names = F) 
allscenarios <- unlist(flat_countries[which(names(flat_countries) == "scenario")], use.names = F) 

#Set up input values
feb2020 <- 3241
postcovid2020 <- 3241+6-2+1

#Flatten this list
flat_countries <- do.call(c, do.call(c, list_countries))

#Create vectors for all sims, all countries, and all scenarios
allsims <- unlist(flat_countries[which(names(flat_countries) == "sim")], use.names = F)
allcountries <- unlist(flat_countries[which(names(flat_countries) == "country")], use.names = F) 
allscenarios <- unlist(flat_countries[which(names(flat_countries) == "scenario")], use.names = F) 

#Now we can loop through all positions in this vector to do this
sim.id <- allsims[1]
country <- allcountries[1]
scenario <- allscenarios[1]

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
  , get_death_rate = T
  , get_asmr = T
  , age_br_months = age_br_months
  , age_labs = age_labs
  , FinalSimYear = FinalSimYear
  , endmo = endmo
  , feb2020 = feb2020
  , path_out = "../Output/"
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


#Now calculate kin counts for the two methods
getKinCountsNew <- function(all_cat = all_cat, kin, kin.type, pid = pid, 
          ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
         ego.female = ego.female, opop = opop, 
         feb2020 = feb2020, postcovid2020 = postcovid2020, sim.id = sim.id) {
  
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
                 category = x, kintype = kin.type, sim.id = sim.id)
  return(output)
  }))

#The second is to find the average number of kin before and after for those who survive: it will return a tibble
kin_bb <- bind_rows(lapply(all_cat, function(x) {
  
  ego.names.bb <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                     attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                     attr(kin2, "ego.dod") >= postcovid2020)]))
  
  num_pre <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), levels = ego.names.bb))
  
  num_post <- table(factor(names(kin2[which(attr(kin2, "still.alive.postcovid2020") == TRUE)]), levels = ego.names.bb))
  
  output <- list(mean_pre = mean(num_pre, na.rm = T), sd_pre = sd(num_pre, na.rm = T), n_survivors = length(num_pre), 
                 mean_post = mean(num_post, na.rm = T), sd_pre = sd(num_post, na.rm = T),
                 category = x, kintype = kin.type, sim.id = sim.id)
  return(output)
}))

kin_both <- list(kin_bm = kin_bm, kin_bb = kin_bb)

return(kin_both)
}

#Run this on all kin types
temp_data <- lapply(1:length(kin), function(x) getKinCountsNew(all_cat = all_cat, 
                                              kin = kin[[x]], kin.type = names(kin[x]), 
                                              pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                                              ego.female = ego.female, opop = opop, 
                                              feb2020 = feb2020, postcovid2020 = postcovid2020, sim.id = sim.id))

#Bind into tibbles
flat_data <- do.call(c, temp_data)
data_bm <- bind_rows(flat_data[which(names(flat_data) %in% "kin_bm")])
data_bb <- bind_rows(flat_data[which(names(flat_data) %in% "kin_bb")])

#Add death_rates
data <- list(kin_bm = data_bm, kin_bb = data_bb, 
             asmr = death_rates[["asmr"]], all_deaths = death_rates[["all_deaths"]])

#Save intermediate output
save(data, file = paste0("~/covid_simulation/Data/sim_results/data", sim.id, ".RData"))

print(paste0("Estimates generated:", sim.id))
