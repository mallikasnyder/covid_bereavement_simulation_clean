#We want to write analysis code that runs quickly
#This is code that uses the sample simulations we developed to 
#generate estimates of kin loss for the two scenarios 
#These files are very large so we want to restrict the number of cohorts of interest early on
#We'll ultimately want a function that loops through each .sup folder, 
#finds each simulation, and applies a set of estimation strategies to it

#4 levels of loops
#1. Looping over directories (getAllEstimates)
#2. Looping over countries (getCountryEstimates)
#3. Looping over simulations (getSimEstimates)
#4. Looping over kin relationships (getKinCounts)

#Source functions and packages
source("01_load_functions.R")

#~~~~~~~~~~PARAMETERS FOR EACH LEVEL (we set these before we start the loops)

#Directories
#PROJDIR <-"~/covid_simulation/" #main project directory
#UNrates2019 = paste0("~/covid_simulation/","UNrates2019",sep='/') #where input rates are kept

# DAG: is it possible to add a parameter to this function that determines how many
# simulations are used per 'run' (num_each)?
#Names of simulation scenarios
dir_stem_list <- c("covid", "other")

#~~~~~~~~~~ANALYSIS BEGINS (See functions for loops)

all_dir_tibbles <- lapply(dir_stem_list, function(x) 
  getAllEstimates(dir_stem = x))

all_data <- bind_rows(all_dir_tibbles)

#Save data
save(all_data, file = "~/covid_simulation/Data/all_data.RData")

