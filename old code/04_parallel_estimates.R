#Making all of this a function so we can parallelize it
estimatekindeathSOCSIM <- function(sims, sim_names) {

#---Loading data and writing functions
#Identifying opop and omar
opop<-sims$opop
omar<-sims$omar

#Adding opop to omar
opop <- add2opop2(sims$opop,omar)

#Filter
#opop <- opop %>%
#  filter(between(cohort, 1800, 2025))

#Person id vector
pid <- opop$pid[opop$cohort == 2014]

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
ego.dob <-unlist(opop[pid, "aod"])
#Vector with months of death of egos
ego.dod <-unlist(opop[pid, "dod"])

#Finding the kin relationships

kin <- getKin(pid = pid)

#Writing this in vectorized form: 
#Ideally we want to vectorize this from the beginning

getVectorizedForm <- function(x) {
  kin2 <- unlist(x, use.names = F)
  names(kin2) <- rep(pid, unlist(lapply(x, length))) #names are the person IDs they are associated with
  attr(kin2, "aod") <- rep(ego.aod, unlist(lapply(x, length))) #adding an attribute for PID age of death
  return(kin2)
}

kin2 <- lapply(kin, getVectorizedForm)

#Age ranges
age_range <- 0:1199 #in months

#Function to find ages at death of various generations
get.agedeath <- function(kin.pid, ego.dob. = ego.dob, opop. = opop) {
  options(warn=-1) #turning off warnings temporarily
  rez<-unlist(mapply(kin.pid,ego.dob,SIMPLIFY=FALSE,
                     FUN=function(kp,eb)ifelse({unlist(opop[kp,"dod"])-eb}>=0, {unlist(opop[kp,"dod"])-eb}, NA))) #negative numbers are NA
  options(warn=0)
  attr(rez, "aod") <- attr(kin.pid, "aod") #ego aod as an attribute
  rez[which(rez > attr(rez, "aod"))] <- NA
  rez <- rez[sapply(rez, is.na) == F]
  return(rez) #returns a list with ages at death for each individual
}

#Creating vectors for function
num_deaths <- c()
num_kin_alive <- c()
ages <- c(paste0("age", age_range))

#Function to create tibbles for number of kin alive and kin death counts
get.deathcounts <- function(kin.type) {
  rez <- get.agedeath(kin.pid = kin2[[kin.type]]) #getting age of death for kin defined
  
  ###Calculating number of kin deaths and kin alive at each age for each PID
  num_deaths <- lapply(age_range, function(y){tapply(rez, names(rez), function(x) sum(x %in% y, na.rm = T))})
  num_kin_alive <- lapply(age_range, function(y){tapply(rez, names(rez), function(x) sum(x > y, na.rm = T))})
  
  #Making these into tibbles
  
  names(num_deaths) <- ages #naming list elements
  
  num_deaths_data <- num_deaths %>%
    as_tibble() %>% #converting this to tibble
    add_column(ego = pid, #adding ego characteristics
               aod = ego.aod,
               dob = ego.dob,
               dod = ego.dod,
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
       file = paste0("../../Data/test_output/", sim_names, "_", kin.type, "_", "output", ".Rsave"))
  
  print(paste0("Saved", " ", sim_names, ":", kin.type))
}

final <- lapply(names(kin2), get.deathcounts)
print("Estimates generated", sim)
}

sim_names <- c("test")


estimatekindeathSOCSIM(sims = sims, sim_names = sim_names)


#Starting to write the code for parallelizing: this doesn't work

#set number of cores
numCores <- 1

#make cluster
cl <- makeCluster(numCores)

#export function to cluster
clusterExport(cl,  
              list("estimatekindeathSOCSIM", "add2opop2", 
                   "getKin", "asYr",
                   "FinalSimYear", "endmo"))

#Using parLapply
parLapply(cl, sims, function(x, sim_names) estimatekindeathSOCSIM(x, sim_names = sim_names))

#stop cluster
stopCluster(cl)

