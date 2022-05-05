#We want to write analysis code that runs quickly
#This is code that uses the sample simulations we developed to 
#generate estimates of kin loss for the two scenarios 
#These files are very large so we want to restrict the number of cohorts of interest early on
#We'll ultimately want a function that loops through each .sup folder, 
#finds each simulation, and applies a set of estimation strategies to it

#Directories
PROJDIR <-"~/covid_simulation/" #main project directory
UNrates2019 = paste0(PROJDIR,"UNrates2019",sep='/') #where input rates are kept

#The UK simulations are in this folder
folder <- findFolders(dir_stem = "covid")[26]

#Find the country name
countryname <- strsplit(folder, "/")
countryname <- gsub(":Medium.sup", "", unlist(countryname))
countryname <- countryname[length(countryname)]

#These are the numbers of the UK simulations
sim.nums <- grep(list.files(path = folder), pattern='.opop$', invert=TRUE, value=TRUE)

#Let's work with the first one for now
load(file = findPath(sim.nums[1]))

dir_stem <- "covid"
sim.num <- sim.nums[1]

# Function parameters for asYr() --------------
FinalSimYear <- 2035 #Are these correct? #The first month is 0, the last month is 3240
#It makes sense we have one extra month in 2035, since our first month (month 1) is actually a February
endmo <- 3420

#Value for the post-covid case
#Number of months for which a country has Covid-adjusted mortality
nmonths <- length(grep(list.files(paste0(UNrates2019, countryname, "/Mort/")), 
                pattern = "Month", invert = F, value = T)) - 2

#Month after Covid-adjusted mortality
postcovid2020 <- feb2020 + nmonths + 1

#---Loading data and writing functions
  #Identifying opop and omar
  opop<-sims$opop
  omar<-sims$omar
  
  #Adding opop to omar
  opop <- add2opop2(sims$opop,omar)
  
  #Filtering to only individuals alive in February 2020: this is month 3241
  #I found that by taking the second unique value of dates of birth for individuals in the 2020 birth cohort
  feb2020 <- 3241

  opop <- opop[opop$dob <= feb2020 & opop$dod > feb2020,]
  
  #Person ID vector
  pid <- opop$pid
  
  #Getting kin for only individuals who survive the Covid period
  pid <- opop$pid[ego.dod > postcovid2020] 
  
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
  ego.aod <- opop$aod[opop$pid %in% pid]
  #Vector with months of birth of egos
  ego.dob <- opop$dob[opop$pid %in% pid]
  #Vector with cohorts of birth of egos
  ego.cohort <- opop$cohort[opop$pid %in% pid]
  #Vector with months of death of egos
  ego.dod <-opop$dod[opop$pid %in% pid]
  #Vector with sex of egos
  ego.female <- opop$fem[opop$pid %in% pid]
  
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
    attr(kin2, "ego.cohort") <- rep(ego.cohort, unlist(lapply(x, length))) #adding an attribute for PID age of death
    attr(kin2, "ego.dod") <- rep(ego.dod, unlist(lapply(x, length))) #adding an attribute for PID age of death
    attr(kin2, "ego.female") <- rep(ego.female, unlist(lapply(x, length))) #adding an attribute for PID sex
    
    kin.dod <- lapply(kin2, function(x) opop$dod[opop$pid %in% x])
    kin.dod[sapply(kin.dod, function(x) length(x)==0)] <- NA_integer_
    attr(kin2, "kin.dod") <- unlist(kin.dod, use.names = F)
    
    attr(kin2, "still.alive.feb2020") <- if_else(attr(kin2, "kin.dod") > feb2020, TRUE, FALSE)
    attr(kin2, "still.alive.postcovid2020") <- if_else(attr(kin2, "kin.dod") > postcovid2020, TRUE, FALSE)
    
    return(kin2)
  }
  
  kin2 <- lapply(kin, getVectorizedForm)
  
  cohorts <- unique(attr(kin2[[2]], "ego.cohort"))
  sex <- unique(attr(kin2[[2]], "ego.female"))
  
  kintest <- kin2[[2]]

  rez <- attr(kin2[[2]], "still.alive.feb2020")
  names(rez) <- attr(kin2[[2]], "names")
  
  num_pre <- unlist(lapply(unique(names(rez)), 
                           function(x) sum(rez[which(names(rez) == x)], na.rm = T)))
  names(num_pre) <- paste0(ego.cohort, "f", ego.sex)
  num_pre_cohort_sex <- unlist(lapply(unique(names(num_pre)), 
                                  function(x) 
                                    mean(num_pre[which(names(num_pre) == x)], na.rm = T))) 
  names(num_pre_cohort_sex) <- unique(names(num_pre))
  
  rez <- attr(kin2[[2]], "still.alive.postcovid2020")
  names(rez) <- attr(kin2[[2]], "names")
  
  num_post <- unlist(lapply(unique(names(rez)), 
                           function(x) sum(rez[which(names(rez) == x)], na.rm = T)))
  names(num_post) <- paste0(ego.cohort, "f", ego.sex)
  num_post_cohort_sex <- unlist(lapply(unique(names(num_post)), 
                                      function(x) 
                                        mean(num_post[which(names(num_post) == x)], na.rm = T))) 
  names(num_post_cohort_sex) <- unique(names(num_post))
  
  col_name_kin <- names(kin2[2])
  
  tibble_pre <- as_tibble(names(num_pre_cohort_sex)) %>%
    add_column(unname(num_pre_cohort_sex)) %>%
    mutate(time = "precovid") %>%
    separate(value, sep="f", into = c("cohort", "sex"))
  
  colnames(tibble_pre)[names(tibble_pre) =="unname(num_pre_cohort_sex)"] <- col_name_kin
  
  tibble_post <- as_tibble(names(num_post_cohort_sex)) %>%
    add_column(unname(num_post_cohort_sex)) %>%
    mutate(time = "postcovid") %>%
    separate(value, sep="f", into = c("cohort", "sex"))
  
  colnames(tibble_post)[names(tibble_post) =="unname(num_post_cohort_sex)"] <- col_name_kin
  
  tibble <- bind_rows(tibble_pre, tibble_post) %>%
    mutate(sim.type = dir_stem, country = countryname, sim.id = sim.num)
    
  

