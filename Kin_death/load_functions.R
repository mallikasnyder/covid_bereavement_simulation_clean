##Loading the functions we want to use
#This does not need to be run separately

setwd("~/covid_bereavement_simulation_clean/Kin_death")

source("functions_bereavement.R")

# Data wrangling
wrangling <- c("tidyverse", "scales", "patchwork", "data.table","parallel", "knitr")

library2(wrangling)
