#Setting directories and loading functions
setwd("~/covid_bereavement_simulation_clean/")

#Source functions and packages
source("~/covid_bereavement_simulation_clean/Kin_death/01_load_functions.R")
source('~/covid_bereavement_simulation_clean/Kin_death/functions_bereavement.R')

#List all files in the directory
allfiles <- list.files("~/covid_bereavement_simulation_clean/Data/estimates/",  full.names = T)

#Load all files and put them into a single object
final_data <- lapply(1:length(allfiles), function(x) {
  try(load(file = allfiles[x]))
  output <- list(kin_ratio = data$kin_ratio, kin_bb = data$kin_bb, death_rates = data$death_rates)
  return(output)
})

#Flatten this list
flattened <- do.call(c, final_data)

#Create the objects
kin_bb <- bind_rows(flattened[which(names(flattened) == "kin_bb")])
death_rates <- bind_rows(flattened[which(names(flattened) == "death_rates")])

#Find how many simulations we have per country and scenario
numbers <- death_rates %>%
  filter(age == "all_sum") %>%
  group_by(country, scenario) %>%
  summarize(nsims = n())
#We can see which countries we're missing a simulation for
#This should not make a big difference for results (on average, a country may be missing one simulation)

#Save each of these objects
save(kin_bb, file = "~/covid_bereavement_simulation_clean/Data/kin_bb.RData")
save(death_rates, file = "~/covid_bereavement_simulation_clean/Data/death_data.RData")
save(numbers, file = "~/covid_bereavement_simulation_clean/Data/numbers.RData")

