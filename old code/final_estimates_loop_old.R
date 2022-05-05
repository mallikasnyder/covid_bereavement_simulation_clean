#This script obtains estimates from the simulation files

setwd("~/covid_simulation/")

#Source functions and packages
source("./Kin_death/01_load_functions.R")

#Directory stems
dir_stem <- c("covid")
USER <- gsub("-ipums", "", system("whoami",intern=TRUE))


#Find details for all simulations
list_countries <- lapply(dir_stem, function(y) {
  scenario <- y
  filepath <- paste0("/90days/", USER, "/", y, "RESULTS/", "SocCovid-mallikasnyder/")
  countries <- list.files(filepath)
  #countries <- grep(countries, pattern = paste0(country_partial, collapse = "|"), invert = F, value = T)
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
feb2020 <- 3241
postcovid2020 <- 3241+6-2+1

#Run simulation analysis scripts
final <- mclapply(1:length(allsims), function(x) 
  try(getEstimates(sim.id = allsims[x], country = allcountries[x], USER = USER, 
                   scenario = allscenarios[x], 
                   feb2020 = feb2020, postcovid2020 = postcovid2020)), mc.cores = 10)
                                                                                                                                 
