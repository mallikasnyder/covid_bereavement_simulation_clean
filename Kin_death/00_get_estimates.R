#This script obtains estimates from the simulation files

setwd("~/covid_bereavement_simulation_clean/")

#Source functions and packages
source("./Kin_death/load_functions.R")

#Directory stems
dir_stem <- c("covid", "other")
USER <- gsub("-ipums", "", system("whoami",intern=TRUE))

#Find details for all simulations
#This is based on a directory structure where
#The simulation outputs are stored in a folder called 90days
#With Covid results in a covidRESULTS folder
#And counterfactual results in an otherRESULTS folder
list_countries <- lapply(dir_stem, function(y) {
  scenario <- y
  filepath <- paste0("/90days/", USER, "/", y, "RESULTS/", "SocCovid-mallikasnyder/")
  countries <- list.files(filepath)
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
postcovid2020 <- 3242+18-2+1

#Run simulation analysis scripts
final <- mclapply(1:length(allsims), function(x) 
  try(getEstimates(sim.id = allsims[x], country = allcountries[x], USER = USER, 
                   scenario = allscenarios[x], 
                   feb2020 = feb2020, postcovid2020 = postcovid2020)), mc.cores = 10)
                                                                                                                                 
