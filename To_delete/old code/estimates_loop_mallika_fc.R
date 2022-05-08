#Modified to test the new code

#Source functions and packages
source('~/covid_simulation/Kin_death/functions_bereavement_fc.R')

# Data wrangling
wrangling <- c("tidyverse", "scales", "patchwork", "data.table","parallel", "knitr")

library2(wrangling)

#Directory stems
dir_stem <- c("other")

#Remove these countries otherwise
country_partial <- c("Russian_Federation", "Iceland", "Croatia", 
                     "Czech_Republic", "Latvia", "Luxembourg", 
                     "Israel", "Lithuania", "Estonia", "Croatia",
                    "Slovakia", "Bulgaria", "Hungary")

#Find details for all simulations
list_countries <- lapply(dir_stem, function(y) {
  scenario <- y
  filepath <- paste0("/90days/mallikasnyder/", y, "RESULTS/", "SocCovid-mallikasnyder/")
  countries <- grep(list.files(filepath), pattern = paste0(country_partial, collapse = "|"), invert = T, value = T)
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

#save(flat_countries, file = "~/covid_simulation/Data/countrylist_excludingincorrect.RData")

#Create vectors for all sims, all countries, and all scenarios
allsims <- unlist(flat_countries[which(names(flat_countries) == "sim")], use.names = F)
allcountries <- unlist(flat_countries[which(names(flat_countries) == "country")], use.names = F) 
allscenarios <- unlist(flat_countries[which(names(flat_countries) == "scenario")], use.names = F) 

#Set up input values
feb2020 <- 3241
postcovid2020 <- 3241+6-2+1

#x <- 50

#system.time(getEstimates(sim.id = allsims[x], country = allcountries[x], scenario = allscenarios[x], 
#            feb2020 = feb2020, postcovid2020 = postcovid2020))

#Run this
#final <- lapply(1, function(x) try(getEstimates(sim.id = allsims[x], country = allcountries[x], scenario = allscenarios[x],
#                                                   feb2020 = feb2020, postcovid2020 = postcovid2020)))

final <- mclapply(1:length(allsims), function(x) 
  try(getEstimates(sim.id = allsims[x], country = allcountries[x], scenario = allscenarios[x], 
                   feb2020 = feb2020, postcovid2020 = postcovid2020)), mc.cores = 10)
