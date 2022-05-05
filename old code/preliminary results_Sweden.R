#Preliminary results for Sweden
#This will give us at the very end vectors that average across simulations to find the estimated number of kin at age 45 for a woman of the cohort of 1980 who is alive at that age
#Not enough loops are used near the end; this can be updated
#However, this code runs fairly quickly

USER <-system("whoami",intern=TRUE) #We need to figure out the directory for this

findSims <- function(dir_stem, USER = USER){
  RESDIR <- paste0("~/90days/", dir_stem, "RESULTS")
  
  Cave <- paste(RESDIR,"/SocCovid-",USER,sep='') #Within this file are all the simulation results
  
  #Now find all folders with simulation results
  countrydir <- list.files(path = Cave, pattern = ".sup$") 
  
  #This presumes all old simulations have been deleted
  sim.folders <- paste(Cave, countrydir, sep = "/")
}  

estimatekindeathSOCSIM_prelim <- function(path, sim_names, cohort.of.interest, age.of.interest){
    load(path)
    #---Loading data and writing functions
    #Identifying opop and omar
    opop<-sims$opop
    omar<-sims$omar
    
    #Adding opop to omar
    opop <- add2opop2(sims$opop,omar)
    
    #Person ID vector
    pid <- opop$pid[opop$cohort %in% cohort.of.interest & opop$fem == 1 & opop$aod >= (age.of.interest+1)*12]
    
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
    
    kin <- getKin(pid = pid)
    
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
    return(final)
  }
  
find.estimates <- function(folder) {
  sim.nums <- grep(list.files(path = folder), pattern='.opop$', invert=TRUE, value=TRUE)
  path <- function(x) {paste(folder, x, "SimResults", "sims.Rsave", sep = "/")}
  est <- lapply(sim.nums, function(x) {unlist(estimatekindeathSOCSIM_prelim(path = path(x), sim_names = x, cohort.of.interest=1972, age.of.interest = 50))})
  return(est)
}
#This is number of people who die after the individual is 50
#So it's the number of people alive when the individual is 49
#cohort.of.interest=1972, age.of.interest = 50
  
covid_folder <- findSims(dir_stem = "covid", USER = USER) 
other_folder <- findSims(dir_stem = "other", USER = USER)  

FinalSimYear <- 2099
endmo <- 4200

estimates_covid <- find.estimates(folder = covid_folder)
estimates_other <- find.estimates(folder = other_folder)

final_covid_mean <- c()
final_covid_max <- c()
final_covid_min <- c()
for (i in (1:length(estimates_covid[[1]]))) {
final_covid_mean[i] <- round(sum(estimates_covid[[1]][i], estimates_covid[[2]][i], estimates_covid[[3]][i], estimates_covid[[4]][i], estimates_covid[[5]][i])/5, 2)
final_covid_max[i] <- round(max(estimates_covid[[1]][i], estimates_covid[[2]][i], estimates_covid[[3]][i], estimates_covid[[4]][i], estimates_covid[[5]][i]), 2)
final_covid_min[i] <- round(min(estimates_covid[[1]][i], estimates_covid[[2]][i], estimates_covid[[3]][i], estimates_covid[[4]][i], estimates_covid[[5]][i]), 2)
}
names(final_covid_mean) <- names(estimates_covid[[1]])
names(final_covid_max) <- names(estimates_covid[[1]])
names(final_covid_min) <- names(estimates_covid[[1]])


final_other_mean <- c()
final_other_max <- c()
final_other_min <- c()
for (i in (1:length(estimates_other[[1]]))) {
  final_other_mean[i] <- round(sum(unlist(estimates_other[[1]][i]), unlist(estimates_other[[2]][i]), unlist(estimates_other[[3]][i]), unlist(estimates_other[[4]][i]), unlist(estimates_other[[5]][i]))/5,2)
  final_other_max[i] <- round(max(estimates_other[[1]][i], estimates_other[[2]][i], estimates_other[[3]][i], estimates_other[[4]][i], estimates_other[[5]][i]),2)
  final_other_min[i] <- round(min(estimates_other[[1]][i], estimates_other[[2]][i], estimates_other[[3]][i], estimates_other[[4]][i], estimates_other[[5]][i]),2)
}

names(final_other_mean) <- names(estimates_other[[1]])
names(final_other_max) <- names(estimates_other[[1]])
names(final_other_min) <- names(estimates_other[[1]])

  