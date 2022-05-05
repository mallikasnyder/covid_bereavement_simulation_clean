#Analyzing estimates loop output for the abstract
#Works with the unnamed simulation datasets that were not unfortunately bound together correctly
#This is a temporary fix

Germany_covid <- c("115313", "131545", "148383", "149658", "153303")
Germany_other <- c("102263", "103310", "106882", "113414", "118346")

Sweden_covid <- c("106737", "12408", "128372", "128990", "143345")
Sweden_other <- c("147228", "151195", "155118", "159199", "173436")

UnitedStates_covid <- c("101745", "103352", "117080", "123638", "126003")
UnitedStates_other <- c("116830", "127103", "133158", "13469", "138")

all_types <- c("Germany_covid", "Germany_other", "Sweden_covid", "Sweden_other", "UnitedStates_covid", "UnitedStates_other")

getDataSalvaged <- function(simtype) {
  keep_sims <- get(simtype)
  sim.identifiers <- simtype
  
  data_saved <- lapply(keep_sims, function(x) 
    get(load(file = paste0("~/covid_simulation/Data/sim_results/", "data", x, ".RData"))))
  
  final_data_type <- bind_rows(data_saved) %>%
    mutate(identity = sim.identifiers) %>%
    separate(identity, into = c("country", "scenario"), sep = "_")
  return(final_data_type)
}

all_salvaged <- lapply(all_types, function(x) getDataSalvaged(simtype = x))

final_data <- bind_rows(all_salvaged)
save(final_data, file = "~/covid_simulation/Data/abstract_data.RData")
                