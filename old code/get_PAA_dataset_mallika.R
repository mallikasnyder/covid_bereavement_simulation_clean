#Setting directories and loading functions
setwd("~/covid_simulation/")

#Source functions and packages
source("~/covid_simulation/Kin_death/01_load_functions.R")
source('~/covid_simulation/Kin_death/functions_bereavement.R')

#List all files in the directory
allfiles <- list.files("~/covid_simulation/Data_old/sim_results_paa/", full.names = T)

#Load all files and put them into a single object
final_data <- lapply(1:length(allfiles), function(x) {
  try(load(file = allfiles[x]))
  output <- list(kin_bm = data$kin_bm, kin_bb = data$kin_bb, death_rates = data$death_rates)
  return(output)
})

#Flatten this list
flattened <- do.call(c, final_data)

#Create the objects
data_bm <- bind_rows(flattened[which(names(flattened) == "kin_bm")])
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
data_paa <- list(kin_bm = data_bm, kin_bb = data_bb, death_rates = data_death_rates)   

#save(data_paa, file = "~/covid_simulation/Data/final_data_paa.RData")
save(data_paa, file = "~/90days/final_data_paa.RData") #Full size dataset
save(data_paa, file = "~/covid_simulation/Data/final_data_paa.RData", compress = T)

#~~~~~~~~~~~~~~~~~EXTRA CODE (DO NOT RUN WHEN GENERATING DATASET)
#Quick check: why do we have estimates generated that do not appear
#4 simulations fail
#13 simulations have the same random seed, so they get over-written
#We will get slightly fewer observations than we should as a result
load("all_data.RData")
length(grep("Estimates generated", final, invert = T, value = T))
#Number of files we should have
final_check <- gsub("Estimates generated:", "", grep("Estimates generated", final, value = T))
#Number of files we actually have
found_files <- gsub("data", "", 
                    gsub(".RData", "", list.files("~/covid_simulation/Data/sim_results_paa/")))
n_occur <- data.frame(table(final_check))

