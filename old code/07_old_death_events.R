#This code is still very much in progress (testing with the "pid" vector)
#Desired outputs:
  #Tibble for number of death events for a particular kin relationship in terms of ego's age
  #Tibble for number of kin still alive for a particular kin relationship in terms of ego's age
  #We can find number of egos alive at a particular age later using age of death
  #These tibbles do not adjust output for ego's age at death (we do that later when creating estimates)

#---Setting this up

#Create an object for testing
test <- res

#Vector of all possible ages
max.aod <- max(unlist(opop[pid, "aod"]))
age_range <- 0:max.aod #in months

#Vector with ages of death of PIDs
aod <- unlist(opop[pid, "aod"])

#Function to find ages at death of various generations
get.agedeath<-function(kin.pid,ego.pid,opop){
  kin.dod<-lapply(kin.pid,function(p){unlist(opop[p,"dod"])}) #month of death of kin
  ego.dob<-lapply(ego.pid,function(p){unlist(opop[p,"dob"])}) #month of birth of ego
  rez<-mapply(kin.dod,ego.dob,SIMPLIFY=FALSE,
              FUN=function(dt,eb)ifelse({dt-eb}>=0, {dt-eb}, NA)) #negative numbers are NA
  options(warn=0)
  return(rez) #returns a list with ages at death for each individual
}
#Move ego.dob outside of the function

kin.pid <- res$gparents

kin.dod<-lapply(kin.pid,function(p){unlist(opop[p,"dod"])}) #month of death of kin
ego.dob<-lapply(ego.pid,function(p){unlist(opop[p,"dob"])}) #month of birth of ego




#Create a sample object using grandparents
rez <- get.agedeath(res$gparents, pid, opop)
#unlist kin.pid and then add a vector if this ends up taking too long

#---Tibble with Number of Death Events by Ego's Age

#Create a list of lists, where each element represents an age at which each ego experiences some number of deaths of a particular type of kin
num_deaths <- list()
tmp <- rez

for (i in 1:length(age_range)) {
for (j in 1:length(pid)) {
tmp[[j]][sapply(tmp[[j]], function(x) x != age_range[[i]])] <- NA
tmp[[j]][sapply(i, function(x) aod[[j]] < age_range[[i]])] <- NA #converting all deaths after ego's death to NA
}
num_deaths[[i]] <- lapply(tmp, function(x) sum(x == age_range[[i]], na.rm = T))
tmp <- rez
}
#unlist everything

#Converting this into a tibble for ease of analysis

names(num_deaths) <- c(paste0("age", age_range)) #naming list elements

num_deaths_data <- num_deaths %>%
  as_tibble() %>% #converting this to tibble
  add_column(ego = pid, #adding ego characteristics
             aod = unlist(opop[pid, "aod"]),
             dob = unlist(opop[pid, "dob"]),
             dod = unlist(opop[pid, "dod"]),
             .before = "age0") %>%
  unnest(age0:age1199) #removing list structure

#---Tibble with Number of Kin Still Alive by Ego's Age 

num_kin_alive <- list()
tmp <- rez

for (i in 1:length(age_range)) {
  for (j in 1:length(pid)) {
    tmp[[j]][sapply(tmp[[j]], function(x) x <= age_range[[i]])] <- NA
    tmp[[j]][sapply(i, function(x) aod[[j]] < age_range[[i]])] <- NA #converting all deaths after ego's death to NA
    
  }
  num_kin_alive[[i]] <- lapply(tmp, function(x) sum(x > age_range[[i]], na.rm = T))
  tmp <- rez
}

#Converting this into a tibble for ease of analysis

names(num_kin_alive) <- c(paste0("age", age_range)) #naming list elements

num_kin_alive_data <- num_kin_alive %>%
  as_tibble() %>% #converting this to tibble
  add_column(ego = pid, #adding ego characteristics
             aod = unlist(opop[pid, "aod"]),
             dob = unlist(opop[pid, "dob"]),
             dod = unlist(opop[pid, "dod"]),
             .before = "age0") %>%
  unnest(age0:age1199) #removing list structure

#---------------Keeping code in storage here
#Create a sample object using grandparents
rez <- get.agedeath(kin.pid = kin2$gparents)

#next step: Tibbles
#Create a list of lists, where each element represents an age at which each ego experiences some number of deaths of a particular type of kin

#Replacing all kin ages with NA where ego has previously died
for (i in 1:length(rez)) {
  rez[[i]] <- sapply(rez[[i]], function(x) ifelse(rez[[i]] > attr(rez, "aod")[[i]], NA, x))
}

###Calculating number of kin deaths at each age for each PID
num_deaths <- c()
for (i in 1:length(age_range)) {
  num_deaths[[i]] <- tapply(rez, names(rez), function(x) sum(x %in% i, na.rm = T))
}

#Making this into a tibble

names(num_deaths) <- c(paste0("age", age_range)) #naming list elements

num_deaths_data <- num_deaths %>%
  as_tibble() %>% #converting this to tibble
  add_column(ego = pid, #adding ego characteristics
             aod = unlist(opop[pid, "aod"]),
             dob = unlist(opop[pid, "dob"]),
             dod = unlist(opop[pid, "dod"]),
             .before = "age0") %>%
  unnest(age0:age1199) #removing list structure

###Calculating number of kin alive at each age for each PID
num_kin_alive <- c()
for (i in 1:length(age_range)) {
  num_kin_alive[[i]] <- tapply(rez, names(rez), function(x) sum(x > i, na.rm = T))
}

#Converting this into a tibble for ease of analysis

names(num_kin_alive) <- c(paste0("age", age_range)) #naming list elements

num_kin_alive_data <- num_kin_alive %>%
  as_tibble() %>% #converting this to tibble
  add_column(ego = pid, #adding ego characteristics
             aod = unlist(opop[pid, "aod"]),
             dob = unlist(opop[pid, "dob"]),
             dod = unlist(opop[pid, "dod"]),
             .before = "age0") %>%
  unnest(age0:age1199) #removing list structure









#----Old code for storage

#Now create two tibbles for each kin relationship in a loop, then save them as Rsave objects
for (i in 2:length(list_of_kin)) { #length(list_of_kin)-currently not running over all options so as to save time
  
  
  rez <- get.agedeath(kin.pid = kin2[[i]]) #getting age of death for kin defined
  
  ###Calculating number of kin deaths and kin alive at each age for each PID
  system.time(for (k in 1:length(age_range)) {
    num_deaths_try[[k]] <- tapply(rez, names(rez), function(x) sum(x %in% k, na.rm = T))
    #num_kin_alive[[k]] <- tapply(rez, names(rez), function(x) sum(x > k, na.rm = T))
  })
  
  num_deaths_test <- lapply(age_range, function(y){tapply(rez, names(rez), function(x) sum(x %in% y, na.rm = T))})
  
  num_deaths_try <- tapply(rez, names(rez), function(x) sum(x %in% y, na.rm = T))  
  
  #Making these into tibbles
  
  names(num_deaths) <- ages #naming list elements
  
  num_deaths_data <- num_deaths %>%
    as_tibble() %>% #converting this to tibble
    add_column(ego = pid, #adding ego characteristics
               aod = ego.aod,
               dob = ego.dob,
               dod = ego.dod,
               .before = "age0") %>%
    unnest(age0:age1199) #removing list structure
  
  names(num_kin_alive) <- ages #naming list elements
  
  num_kin_alive_data <- num_kin_alive %>%
    as_tibble() %>% #converting this to tibble
    add_column(ego = pid, #adding ego characteristics
               aod = ego.aod,
               dob = ego.dob,
               dod = ego.dod,
               .before = "age0") %>%
    unnest(age0:age1199) #removing list structure
  
  kin_tibbles <- list(num_deaths_data, num_kin_alive_data)
  
  #Saving these together as Rsave
  save(kin_tibbles,
       file = paste0("../../Data/test_output/", list_of_kin[i], "_", "output", ".Rsave"))
}

