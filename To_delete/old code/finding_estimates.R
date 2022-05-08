#We want to write analysis code that runs quickly
#This is code that uses the sample simulations we developed to 
#generate estimates of kin loss for the two scenarios 
#These files are very large so we want to restrict the number of cohorts of interest early on
#We'll ultimately want a function that loops through each .sup folder, 
#finds each simulation, and applies a set of estimation strategies to it

#The UK simulations are in this folder
folder <- findFolders(dir_stem = "covid")[26]

#These are the numbers of the UK simulations
sim.nums <- grep(list.files(path = folder), pattern='.opop$', invert=TRUE, value=TRUE)

#Let's work with the first one for now
load(file = findPath(sim.nums[1]))

# Function parameters --------------
FinalSimYear <- 2034 #Are these correct?
endmo <- 3420


#---Loading data and writing functions
  #Identifying opop and omar
  opop<-sims$opop
  omar<-sims$omar
  
  #Adding opop to omar
  opop <- add2opop2(sims$opop,omar)
  
  #Filtering to only individuals alive in February 2020: this is month 3241
  #I found that by taking the second unique value of dates of birth for individuals in the 2020 birth cohort
  opop <- opop[opop$dob <= 3241 & opop$dod >= 3241,]
  
  #Person ID vector
  pid <- opop$pid
  
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
  
  #Vector with ages of death of PIDs
  ego.aod <- unlist(opop[pid, "aod"])
  #Vector with months of birth of egos
  ego.dob <-unlist(opop[pid, "dob"])
  #Vector with months of death of egos
  ego.dod <-unlist(opop[pid, "dod"])
  
  #Finding the kin relationships
  all.kin.types <- c("g0", "gparents", "gunclesaunts",
                     "parents", "unclesaunts",
                     "siblings", "firstcousins", "children",
                     "spouse", "parentsofspouse", "brothersisterinlaw")
  
  kin <- getKinEach(pid = pid, kintype = all.kin.types[6])
  
  #Writing this in vectorized form: 
  #Ideally we want to vectorize this from the beginning
  

  
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
  
  kin2 <- lapply(kin, getVectorizedForm)
  
  #Function to create tibbles for number of kin alive and kin death counts
  get.deathcounts <- function(kin.type) {
    
    rez <- attr(kin2[[kin.type]], "kin.aod.years")
    names(rez) <- attr(kin2[[kin.type]], "names")
    
    ###Calculating number of kin deaths and kin alive at each age for each PID
    num_kin_alive <- unlist(lapply(unique(names(rez)), function(x) sum(rez[which(names(rez) == x)] > age.of.interest, na.rm = T)))
    print(paste(kin.type, mean(num_kin_alive)))
    mean <- mean(unlist(num_kin_alive))
    names(mean) <- kin.type
    return(mean)
  }
  
  final <- lapply(names(kin2)[-1], get.deathcounts)
  print(paste("Estimates generated", sim_names))

