#################################
#Code for Analyzing SOCSIM Output
#################################

#List all files in directory
(files <- list.files(pattern = ".R$")[-1])

#Loading functions and packages
source(files[1])

#Loading data and parameters
source(files[2])

#Identifying kin relationships
source(files[3])

#Finding death events by kin relationship (for calculating rates)
source(files[4])

#Estimating rates and visualizing them
source(files[5])