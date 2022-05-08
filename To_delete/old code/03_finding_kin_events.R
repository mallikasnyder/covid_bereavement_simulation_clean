#Adapted from Carl Mason's code
#Currently I'm identifying the kin relationships and placing each relationship thus derived in a nested list, res
#See this chart for definitions: https://www.familysearch.org/blog/en/cousin-chart/

#---Loading data and writing functions

#Identifying opop and omar
opop<-sims$opop
omar<-sims$omar

#Adding opop to omar
opop <- add2opop2(sims$opop,omar)

#Filter
#opop <- opop %>%
#  filter(between(cohort, 1800, 2025))

#Defining the cohorts of interest
pid<-opop[opop$cohort %in% c(1980),"pid"]

### FUNCTION for finding children
kidsOf<-with(opop,{c(
  tapply(pid,mom,c),
  tapply(pid,pop,c)
)})

#Using indexes saves space
#We have to run both of these since there are two 0s
kidsOf["0"]<-NULL;  kidsOf["0"]<-NULL
KidsOf<-list()
KidsOf[as.numeric(names(kidsOf))]<-kidsOf

#Final version of function
ko <- function(p){
  lapply(p,function(x){unique(as.vector(unlist(KidsOf[x])))})
}

### FUNCTION for finding ego's spouse
so <- function(p){
  lapply(p,function(p){as.vector(unlist(opop[opop$pid %in% p,
                                             c("spouse")]))})
}

#Vector with ages of death of PIDs
ego.aod <- unlist(opop[pid, "aod"])
#Vector with months of birth of egos
ego.dob <-unlist(opop[pid, "dob"])
#Vector with months of death of egos
ego.dod <-unlist(opop[pid, "dod"])

#Finding the kin relationships
#Right now I'm just running it for grandparents

system.time(kin <- getKin_gparents(pid = pid))
#1.5 minutes to find grandparents for one simulation

#Writing this in vectorized form: 
#Ideally we want to vectorize this from the beginning

getVectorizedForm <- function(x) {
  kin2 <- unlist(x, use.names = F)
  names(kin2) <- rep(pid, unlist(lapply(x, length))) #names are the person IDs they are associated with
  attr(kin2, "aod") <- rep(ego.aod, unlist(lapply(x, length))) #adding an attribute for PID age of death
  attr(kin2, "dob") <- rep(ego.dob, unlist(lapply(x, length))) #adding an attribute for PID date of birth
  attr(kin2, "dod") <- rep(ego.dod, unlist(lapply(x, length))) #adding an attribute for PID date of death
  return(kin2)
}

kin2 <- lapply(kin[-1], getVectorizedForm)

#Age ranges
#age_range <- 0:1199 #in months

#Function to find ages at death of various generations
get.agedeath <- function(kin.pid, ego.dob. = ego.dob, opop. = opop) {
  options(warn=-1) #turning off warnings temporarily
    rez<-mapply(kin.pid,ego.dob,SIMPLIFY=FALSE,
                FUN=function(kp,eb)ifelse({unlist(opop[kp,"dod"])-eb}>=0, {unlist(opop[kp,"dod"])-eb}, NA)) #negative numbers are NA
  options(warn=0)
  nulls <- sapply(rez, function(x) length(x) == 0)
  rez <- rez[nulls == F]
  rez <- unlist(rez)
  attr(rez, "aod") <- unlist(opop[as.numeric(names(rez)),"aod"]) #ego aod as an attribute
  rez[which(rez > attr(rez, "aod"))] <- NA
  attr(rez, "fem") <- unlist(opop[as.numeric(names(rez)),"fem"]) #ego fem (the sex variable) as an attribute
  attr(rez, "dob") <- unlist(opop[as.numeric(names(rez)),"dob"]) #ego fem (the sex variable) as an attribute
  
  #Another option to save space:
  #rez <- rez[is.na(rez) == F] # we lose all attributes but names but that's okay
  #This might be needed later
return(rez) #returns a list with ages at death for each individual
}




#get.agedeath.tibble <- function(kin.pid, ego.dob. = ego.dob, opop. = opop) {
  options(warn=-1) #turning off warnings temporarily
  rez<-mapply(kin.pid,ego.dob,SIMPLIFY=FALSE,
              FUN=function(kp,eb)ifelse({unlist(opop[kp,"dod"])-eb}>=0, {unlist(opop[kp,"dod"])-eb}, NA)) #negative numbers are NA
  options(warn=0)
  nulls <- sapply(rez, function(x) length(x) == 0)
  rez <- rez[nulls == F]
  rez <- unlist(rez)
  aod <- unlist(opop[as.numeric(names(rez)),"aod"]) #ego aod as an attribute
  fem <- unlist(opop[as.numeric(names(rez)),"fem"]) #ego fem (the sex variable) as an attribute
  dob <- unlist(opop[as.numeric(names(rez)),"dob"]) #ego fem (the sex variable) as an attribute
  
  rez.tibble <- as_tibble(rez) %>%
    add_column(ego = as.numeric(names(rez)),
               aod = aod,
               fem = fem,
               dob = dob) %>%
    mutate(death_year = if_else(value <= aod, value, NA_integer_),
           sim_id = sim.name) #Is the NA_integer_ thing necessary?

  #Another option to save space:
  #rez <- rez[is.na(rez) == F] # we lose all attributes but names but that's okay
  #This might be needed later
  return(rez.tibble) #returns a tibble with ages at death for each individual
}

sim.name = "Austria"
rez.alt <- get.agedeath.tibble(kin.pid = kin2[[kin.type]])



#Creating vectors for function
num_deaths <- c()
num_kin_alive <- c()

#ages <- c(paste0("age", age_range))
year_groups <- seq(0, 1188, by = 12)
age_groups <- year_groups/12
ages <- c(paste0("age", year_groups/12))
#age_range_year <- cut(age_range, breaks = c(year_groups, Inf), right = F, labels = year_groups)

#Function to create tibbles for number of kin alive and kin death counts
get.deathcounts <- function(kin.type, opop = opop) {
  rez <- get.agedeath(kin.pid = kin2[[kin.type]]) #getting age of death for kin defined
  
  ###Calculating number of kin deaths and kin alive at each age for each PID
  num_deaths <- lapply(year_groups, function(y)
    {tapply(rez, as.numeric(names(rez)), function(x) sum(trunc(x/12) %in% y, na.rm = T))})
  num_kin_alive <- lapply(year_groups, function(y){tapply(rez, as.numeric(names(rez)), function(x) sum(x > y+12, na.rm = T))})
  #This is taking too long: is there a faster way?
  
  #num_deaths_test <- lapply(year_groups, function(y){tapply(rez, as.numeric(names(rez)), function(x) sum(between(x, y, y+12), na.rm = T)})
  
  pid.list <- unique((attr(rez, "names")))
  num_deaths_test <- lapply(year_groups, function(y){rowSums()})
  
  num_deaths_test <- mapply(year_groups, unique(attr(rez, "dob")), unique(attr(rez, "fem")), SIMPLIFY = T,
                            FUN = function(yg, dob, fem) 
                              {sum(trunc((rez[which(attr(rez, "dob") %in% dob & 
                                                      attr(rez, "fem") %in% fem)]/12), 
                                         na.rm = T))})
  
  num_deaths_test <- lapply(unique(attr(rez, "dob")), function(y) {unlist(lapply(year_groups, 
                function(x) {sum(trunc(rez[which(attr(rez, "dob") %in% dob)]/12) %in% y, 
                                na.rm = T)}))})
  
  
  num_deaths_test <- mapply(pid.list, year_groups, FUN = function(x,y) 
    {sum(trunc(rez[which(names(rez) %in% y)]/12) %in% y, na.rm = T)}, simplify = F)
  
  
  for (i in 1:length(year_groups)) {
    num_deaths[i] <- lapply(unique(names(rez)), function(x) 
      sum(trunc(rez[which(names(rez) == x)]/12) %in% age_groups[i], na.rm = T))
  }
  
  #Making these into tibbles
  
  names(num_deaths) <- ages #naming list elements
  
  names <- as.numeric(names(num_deaths[[1]]))
  aod <- unlist(opop[names,"aod"]) #ego.aod as an attribute
  dob <- unlist(opop[names,"dob"]) #ego.dob as an attribute
  dod <- unlist(opop[names,"dod"]) #ego.dod as an attribute
  
  
  num_deaths_data <- num_deaths %>%
    as_tibble() %>% #converting this to tibble
    add_column(ego = names, #adding ego characteristics
               aod = aod,
               dob = dob,
               dod = dod,
               .before = "age0") %>%
    unnest(age0:age99) #removing list structure
  
  names(num_kin_alive) <- ages #naming list elements
  
  num_kin_alive_data <- num_kin_alive %>%
    as_tibble() %>% #converting this to tibble
    add_column(ego = pid, #adding ego characteristics
               aod = ego.aod,
               dob = ego.dob,
               dod = ego.dod,
               .before = "age0") %>%
    unnest(age0:age99) #removing list structure
  
  kin_tibbles <- list(num_deaths_data, num_kin_alive_data)
  
  #Saving these together as Rsave
  save(kin_tibbles,
       file = paste0("../../Data/test_output/", kin.type, "_", "output", ".Rsave"))

  print(paste0("Saved", ":", kin.type))
  }

#Run this with the grandparents we want
system.time(lapply(names(kin2)[2], function(x) get.deathcounts(x)))

calculate_deaths <- rez %>%
  group_by


rez.tibble <- rez %>%
  as_tibble()