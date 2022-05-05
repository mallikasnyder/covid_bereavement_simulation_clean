#Setting directories and loading functions
setwd("~/covid_simulation/")

#Source functions and packages
source("~/covid_simulation/Kin_death/01_load_functions.R")
source('~/covid_simulation/Kin_death/functions_bereavement.R')

#List all files in the directory
allfiles <- list.files("~/covid_simulation/Data/estimates/",  full.names = T)


#Load all files and put them into a single object
final_data <- lapply(1:length(allfiles), function(x) {
  try(load(file = allfiles[x]))
  output <- list(kin_ratio = data$kin_ratio, kin_bb = data$kin_bb, death_rates = data$death_rates)
  return(output)
})

#Flatten this list
flattened <- do.call(c, final_data)

#Create the objects
data_ratio <- bind_rows(flattened[which(names(flattened) == "kin_ratio")])
data_bb <- bind_rows(flattened[which(names(flattened) == "kin_bb")])
data_death_rates <- bind_rows(flattened[which(names(flattened) == "death_rates")])

#Find how many simulations we have per country and scenario
find_numbers <- data_death_rates %>%
  filter(age == "all_sum") %>%
  group_by(country, scenario) %>%
  summarize(nsims = n())
#We can see which countries we're missing a simulation for
#This should not make a big difference for results (on average, a country may be missing one simulation)

#Create an object with the three data tibbles
data_list <- list(kin_ratio = data_ratio, kin_bb = data_bb, death_rates = data_death_rates, numbers = find_numbers)   
#Save data
save(data_list, file = "~/covid_simulation/Data/finaldata_monthly_v2.RData", compress = T)



