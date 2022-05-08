#We want to write analysis code that runs quickly
#This is code that uses the sample simulations we developed to 
#generate estimates of kin loss for the two scenarios 
#These files are very large so we want to restrict the number of cohorts of interest early on
#We'll ultimately want a function that loops through each .sup folder, 
#finds each simulation, and applies a set of estimation strategies to it

#4 levels of loops
#Looping over directories
#Looping over countries
#Looping over simulations
#Looping over kin relationships


#~~~~~~~~~~PARAMETERS FOR EACH LEVEL (we set these before we start the loops)

#Directories
PROJDIR <-"~/covid_simulation/" #main project directory
UNrates2019 = paste0(PROJDIR,"UNrates2019",sep='/') #where input rates are kept

#Names of simulation scenarios
dir_stem <- c("covid") #, "other")

#Kin relationship types to loop over
all.kin.types <- c("g0", "gparents", "gunclesaunts",
                   "parents", "unclesaunts",
                   "siblings", "firstcousins", "children",
                   "spouse", "parentsofspouse", "brothersisterinlaw")

# Function parameters for asYr()
FinalSimYear <- 2035 #Are these correct? #The first month is 0, the last month is 3240
#It makes sense we have one extra month in 2035, since our first month (month 1) is actually a February
endmo <- 3420

#Date for last month pre-covid
feb2020 <- 3241

#~~~~~~~~~~ANALYSIS BEGINS

#~~~~~~~~~~Level 1: Looping over directories (scenarios)

for (i in 1:length(dir_stem)) {

#~~~~~~~~~~Level 2: Looping over countries

#Simulations are in this folder
#This function presumes covid simulations are stored in Mallika Snyder's 90 days folder, 
#and counterfactual simulations in Diego Alburez-Gutierrez's 90days folder

#The simulations are in these folders
folders <- findFolders(dir_stem = dir_stem[i])

for (j in 1:length(folders)) {
  #Choose a country
  folder <- folders[j]
  
  #Find the country name
  countryname <- strsplit(folder, "/")
  countryname <- gsub(":Medium.sup", "", unlist(countryname))
  countryname <- countryname[length(countryname)]
  
  #Find number of months for which a country has Covid-adjusted mortality
  nmonths <- length(grep(list.files(paste0(UNrates2019, countryname, "/Mort/")), 
                         pattern = "Month", invert = F, value = T)) - 2
  
  #Month after Covid-adjusted mortality
  postcovid2020 <- feb2020 + nmonths + 1
  
  #Find the numbers of the simulations
  sim.nums <- grep(list.files(path = folder), pattern='.opop$', invert=TRUE, value=TRUE)
  
  #~~~~~~~~~~~~Level 3:Looping over simulations
  
  for (k in 1:length(sim.nums)) {
    #Identify the simulation
    sim.id <- sim.nums[k]
    
    #Now load the data
    load(file = findPath(sim.id))
    
    #Identifying opop and omar
    opop<-sims$opop
    omar<-sims$omar
    
    #Adding opop to omar
    opop <- add2opop2(sims$opop,omar)
    
    #Filtering to only individuals alive in February 2020: this is month 3241
    #I found that by taking the second unique value of dates of birth for individuals in the 2020 birth cohort
    opop <- opop[opop$dob <= feb2020 & opop$dod > feb2020,]
    
    #Person ID vector for all egos
    pid <- opop$pid
    
    #Filtering so we get kin for only individuals who survive the Covid period
    pid <- pid[opop$dod > postcovid2020] 
    
    #Vector with ages of death of PIDs
    ego.aod <- opop$aod[opop$pid %in% pid]
    #Vector with months of birth of egos
    ego.dob <- opop$dob[opop$pid %in% pid]
    #Vector with cohorts of birth of egos
    ego.cohort <- opop$cohort[opop$pid %in% pid]
    #Vector with months of death of egos
    ego.dod <-opop$dod[opop$pid %in% pid]
    #Vector with sex of egos
    ego.female <- opop$fem[opop$pid %in% pid]
    
    #Preparing to find kin relationships
    # Function for finding children
    kidsOf<-with(opop,{c(
      tapply(pid,mom,c),
      tapply(pid,pop,c)
    )})
    
    #Using indexes saves space
    #We have to run both of these since there are two 0s
    kidsOf["0"]<-NULL;  kidsOf["0"]<-NULL
    KidsOf<-list()
    KidsOf[as.numeric(names(kidsOf))]<-kidsOf
    
    #~~~~~~~~~~~~Level 4: Looping over kin relationships
    #Run the analysis code on all kin relationships
    all_tibbles <- lapply(all.kin.types, function(x) getKinCounts(pid = pid, kin.name = x))
    
    #Now bind these rows and add some id variables
    data <- bind_rows(all_tibbles) %>%
      mutate(sim.type = dir_stem, country = countryname, sim.id = sim.num)
    
    }
  }
}







