#Now that we have the dataframes we'd like to find, we can think of ways of representing them
#This is still using the small test dataset

#---Rate 1: Rate of death of kin type

#numerator: num_deaths
#denominator: num_kin_alive

#colMeans rm NA = T

#Numerator
#For each age, calculate column sums of num_deaths for all egos alive at that age
num_deaths_vec <- colSums(num_deaths_data[-c(1:4)])

#Denominator
#For each age, calculate column sums of num_kin_alive for all egos alive at that age
num_kin_alive_vec <- colSums(num_kin_alive_data[-c(1:4)]) 

rate_kin_death <- num_deaths_vec %>%
  as_tibble_col(column_name = "num_deaths") %>%
  mutate(num_deaths = unname(num_deaths)) %>%
  add_column(num_kin_alive = unname(num_kin_alive_vec),
             age = age_range, .before = "num_deaths") %>%
  mutate(rate_kin_death = num_deaths/num_kin_alive)

rate_kin_death %>%
  ggplot() + geom_point(aes(x = age_range, y = rate_kin_death)) +
  labs(x = "Age of Ego in Months", y = "Rate of kin death")
  
#---Rate 2: Rate of bereavement

#numerator: number of deaths (same as before)
#One thing we'll want to think about is whether the adjustment to include only kin who are alive before the ego's death makes sense in both cases

#denominator: number of egos still alive
#Create 1200-element vector of number of egos alive at each age
num_ego_alive_vec <- c()

for (i in 0:max(age_range)) {
  num_ego_alive_vec[i+1] <- sum(aod>i)
}

#Wrapping this into dataframe
rate_bereavement <- num_deaths_vec %>%
  as_tibble_col(column_name = "num_deaths") %>%
  mutate(num_deaths = unname(num_deaths)) %>%
  add_column(num_ego_alive = num_ego_alive_vec,
             age = age_range, .before = "num_deaths") %>%
  mutate(rate_of_bereavement = num_deaths/num_ego_alive)

rate_bereavement %>%
  ggplot() + geom_point(aes(x = age_range, y = rate_of_bereavement)) +
  labs(x = "Age of Ego in Months", y = "Rate of Bereavement")


#Going forward:
#1. Currently for only one cohort.
#2. Currently for only one kin relationship (grandparents).
#3. Currently only for 624 PIDs
#4. Should we measure kin deaths after the ego dies?
#5. Write more of this as functions/loops and find cleaner ways of doing this.
#6. Removing objects
#Garbage collector in R after removing objects (command: gc())
#parallellize at the simulation level (mcapply() rather than dopar())
#vectorize as much as possible: eg. not using a loop, but saving functions estimateChildDeathSocsim line 718
#save things as csvs/lists
#array

#Old version of this function
getVectorizedForm <- function(x) {
  kin2 <- unlist(x, use.names = F)
  names(kin2) <- rep(pid, unlist(lapply(x, length))) #names are the person IDs they are associated with
  attr(kin2, "ego.aod") <- rep(ego.aod, unlist(lapply(x, length))) #adding an attribute for PID age of death
  attr(kin2, "ego.dob") <- rep(ego.dob, unlist(lapply(x, length))) #adding an attribute for PID age of death
  attr(kin2, "ego.dod") <- rep(ego.dod, unlist(lapply(x, length))) #adding an attribute for PID age of death
  
  attr(kin2, "kin.dod") <- unlist(opop[kin2, "dod"])
  attr(kin2, "kin.aod.years") <- if_else(between(attr(kin2, "kin.dod")-attr(kin2, "ego.dob"), 0, attr(kin2, "ego.aod")), trunc((attr(kin2, "kin.dod")-attr(kin2, "ego.dob"))/12), NA_real_) 
  attr(kin2, "kin.aod.months") <- if_else(between(attr(kin2, "kin.dod")-attr(kin2, "ego.dob"), 0, attr(kin2, "ego.aod")), (attr(kin2, "kin.dod")-attr(kin2, "ego.dob")), NA_integer_) 
  attr(kin2, "still.alive") <- if_else(attr(kin2, "kin.aod.years") > age.of.interest, 1, NA_real_)
  return(kin2)
}







