###This script contains old/defunct versions of the functions used in functions_bereavement.R

#--------Function to find folder names that house all the simulations for a given country
findFolders <- function(dir_stem){
  RESDIR <- paste0("~/90days/", dir_stem, "RESULTS")
  
  # DAG: changed this to make it more generalisable
  # USER <- "mallikasnyder" #All simulations copied to this 90days folder
  USER <- Sys.info()["user"]
  
  Cave <- paste(RESDIR,"/SocCovid-",USER,sep='') #Within this file are all the simulation results
  
  #Now find all folders with simulation results
  countrydir <- list.files(path = Cave, pattern = ".sup$") 
  
  #Limit to certain countries for now
  abstract_countrynames <- c("Germany", "Sweden", "United_States_of_America")
  
  #Now limit countrydir
  countrydir <- grep(countrydir, pattern = paste(abstract_countrynames,collapse="|"), value = T)
  
  #This presumes all old simulations have been deleted
  sim.folders <- paste(Cave, countrydir, sep = "/")
  return(sim.folders)
}  




#Old version with extra kin relationships
getKinTogetherold <- function(pid) {
  #Creating a list to hold these various kin relationships
  res<-list(
    ## ego is in g3 
    pid = pid)
  
  
  #--- The family tree above ego
  ##Great grandparents
  res$g0=lapply(pid,
                function(pid){
                  as.vector(unlist(opop[opop$pid == pid ,
                                        c("MMM","MMF","MFM","MFF",
                                          "FMM","FMF","FFM","FFF")]))})
  
  ##Grandparents, great-uncles, great-aunts
  res$g1 = ko(res$g0)
  
  #Grandparents
  res$gparents=lapply(pid,
                      function(pid){
                        as.vector(unlist(opop[opop$pid == pid,
                                              c("MM","MF","FM","FF")]))})
  
  #Great-uncles and aunts
  res$gunclesaunts = lapply(seq_along(res$g1), 
                            function(i) res$g1[[i]][res$g1[[i]] %ni% res$gparents[[i]]])
  
  ## Parents, uncles, and aunts
  res$g2=ko(res$gparents)
  
  #Parents
  res$parents=lapply(pid,
                     function(pid){
                       as.vector(unlist(opop[opop$pid == pid,
                                             c("mom","pop")]))})
  #Uncles and Aunts
  res$unclesaunts = lapply(seq_along(res$g2), function(i) res$g2[[i]][res$g2[[i]] %ni% res$parents[[i]]])
  
  #--The family tree at ego’s generation (g3)
  
  ##Cousins and Siblings
  res$g3 = ko(res$g2)
  
  #Siblings
  res$siblings = ko(res$parents)
  #making sure to remove ego
  res$siblings = lapply(seq_along(res$siblings), function(i) res$siblings[[i]][res$siblings[[i]] %ni% pid[[i]]])
  
  #First cousins
  res$firstcousins = ko(res$unclesaunts)
  
  #--The family tree below ego’s generation (g4 etc)
  
  ## Children, Nieces, and Nephews
  res$g4=ko(res$g3)
  
  #Children
  res$children = ko(pid)
  
  #Nieces and nephews
  res$niecesnephews = ko(res$siblings)
  
  ## Grandchildren, grand-nieces, grand-nephews
  #Grandchildren
  res$gchildren = ko(res$children)
  
  #Grand-nieces and grand-nephews
  res$gniecesnephews = ko(res$niecesnephews)
  
  ## Great-grand children
  res$ggchildren = ko(res$gchildren)
  
  ###Adding more distant relatives
  
  #First cousins, once removed: children of great uncles/aunts and also children of first cousins
  res$childrengunclesaunts = ko(res$gunclesaunts)
  res$childrenfirstcousins = ko(res$firstcousins)
  res$firstcousins1rm = mapply(c, res$childrengunclesaunts, res$childrenfirstcousins, SIMPLIFY=F)
  
  #First cousins twice removed: grandchildren of first cousins
  res$firstcousins2rm = ko(res$childrenfirstcousins)
  
  #Second cousins
  res$secondcousins = ko(res$childrengunclesaunts)
  
  #Second cousins, once removed
  res$secondcousins1rm = ko(res$secondcousins)
  
  #Second cousins, twice removed
  res$secondcousins2rm = ko(res$secondcousins1rm)
  
  #--In-laws
  
  #Spouse
  res$spouse <- so(pid)
  
  #Parents-in-law
  res$parentsofspouse=lapply(res$spouse,
                             function(x){
                               unlist(opop[opop$pid %in% x,
                                           c("mom","pop")])})
  
  #Brothers and sister in laws = siblings of spouse/spouse of siblings
  #Spousesofsiblings
  
  res$spouseofsiblings <- so(res$siblings)
  res$siblingsofspouse <- ko(res$parentsofspouse)
  res$siblingsofspouse <- lapply(seq_along(res$siblingsofspouse), 
                                 function(i) res$siblingsofspouse[[i]][res$siblingsofspouse[[i]] %ni% res$spouse[[i]]])
  res$brothersisterinlaw <- mapply(c, res$spouseofsiblings, res$siblingsofspouse, SIMPLIFY=F)
  
  #--Data cleaning
  #Replacing NULLs and integer(0) with NA
  #I'm not sure if this is necessary but I do it here in case it is helpful later.
  
  for (i in 1:length(names(res))) {
    res[[i]][sapply(res[[i]], function(x) length(x)==0)] <- NA #removing NULLs and integer(0) with NA
    #res[[i]] <- sapply(res[[i]], function(x) unname(x)) #Removing names for space storage
  }
  
  return(res)
}



#This function is much less parsimonious in terms of number of lines: it calculates a specified kin relationship for a set of PIDs
#However, this is likely to be better for long vectors of PIDs 
getKinEach <- function(opop = opop, KidsOf = KidsOf, pid, kintype) {
  #See this chart for definitions: https://www.familysearch.org/blog/en/cousin-chart/
  
  #This is where results are stored: we want the first object to be PIDs
  res<-list() ## ego is in g3 (Removed pid from this)
  
  #--- The family tree above ego
  ##Great grandparents
  if (kintype %in% "g0"){
    res$g0=lapply(pid,
                  function(pid){
                    as.vector(unlist(opop[opop$pid == pid ,
                                          c("MMM","MMF","MFM","MFF",
                                            "FMM","FMF","FFM","FFF")]))})
  }
  
  ##Grandparents, great-uncles, great-aunts
  if (kintype %in% "g1"){
    g0=lapply(pid,
              function(pid){
                as.vector(unlist(opop[opop$pid == pid ,
                                      c("MMM","MMF","MFM","MFF",
                                        "FMM","FMF","FFM","FFF")]))})
    res$g1 = ko(KidsOf = KidsOf, p = g0)
  }
  
  #Grandparents
  if (kintype %in% "gparents"){
    res$gparents=lapply(pid,
                        function(pid){
                          as.vector(unlist(opop[opop$pid == pid,
                                                c("MM","MF","FM","FF")]))})
  }
  
  if (kintype %in% "gunclesaunts"){
    #Grand-uncles and aunts
    g0=lapply(pid,
              function(pid){
                as.vector(unlist(opop[opop$pid == pid ,
                                      c("MMM","MMF","MFM","MFF",
                                        "FMM","FMF","FFM","FFF")]))})
    g1 = ko(KidsOf = KidsOf, p = g0)
    gparents=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                            c("MM","MF","FM","FF")]))})
    res$gunclesaunts = lapply(seq_along(g1), 
                              function(i) g1[[i]][g1[[i]] %ni% gparents[[i]]])
  }
  
  ## Parents, uncles, and aunts
  if (kintype %in% "g2") {
    gparents=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                            c("MM","MF","FM","FF")]))})
    res$g2=ko(KidsOf = KidsOf, p = gparents)
  }
  
  #Parents
  if (kintype %in% "parents"){
    res$parents=lapply(pid,
                       function(pid){
                         as.vector(unlist(opop[opop$pid == pid, 
                                               c("mom","pop")]))})
  }
  
  #Uncles and Aunts
  if (kintype %in% "unclesaunts") {
    gparents=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                            c("MM","MF","FM","FF")]))})
    g2=ko(KidsOf = KidsOf, p = gparents)
    parents=lapply(pid,
                   function(pid){
                     as.vector(unlist(opop[opop$pid == pid, 
                                           c("mom","pop")]))})
    res$unclesaunts = lapply(seq_along(g2), function(i) g2[[i]][g2[[i]] %ni% parents[[i]]])
  }
  
  #--The family tree at ego’s generation (g3)
  
  ##Cousins and Siblings
  if (kintype %in% "g3") {
    gparents=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                            c("MM","MF","FM","FF")]))})
    g2=ko(KidsOf = KidsOf, p = gparents)
    res$g3 = ko(KidsOf = KidsOf, p = g2)
  }
  
  #Siblings
  if (kintype %in% "siblings"){
    parents=lapply(pid,
                   function(pid){
                     as.vector(unlist(opop[opop$pid == pid, 
                                           c("mom","pop")]))})
    res$siblings = ko(KidsOf = KidsOf, p = parents)
    
    #Making sure to remove ego
    res$siblings = lapply(seq_along(res$siblings), function(i) res$siblings[[i]][res$siblings[[i]] %ni% pid[[i]]])
  }
  
  if (kintype %in% "firstcousins"){
    #First cousins
    gparents=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                            c("MM","MF","FM","FF")]))})
    g2=ko(KidsOf = KidsOf, p = gparents)
    parents=lapply(pid,
                   function(pid){
                     as.vector(unlist(opop[opop$pid == pid, 
                                           c("mom","pop")]))})
    unclesaunts = lapply(seq_along(g2), function(i) g2[[i]][g2[[i]] %ni% parents[[i]]])
    res$firstcousins = ko(KidsOf = KidsOf, p = unclesaunts)
  }
  
  #--The family tree below ego’s generation (g4 etc)
  
  ## Children, Nieces, and Nephews
  if (kintype %in% "g4"){
    gparents=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                            c("MM","MF","FM","FF")]))})
    g2=ko(KidsOf = KidsOf, p = gparents)
    g3 = ko(KidsOf = KidsOf, p = g2)
    res$g4=ko(KidsOf = KidsOf, p = g3)
  }
  
  #Children
  if (kintype %in% "children"){
    res$children = ko(KidsOf = KidsOf, p = pid)
  }
  
  #Nieces and nephews
  if (kintype %in% "niecesnephews"){
    parents=lapply(pid,
                   function(pid){
                     as.vector(unlist(opop[opop$pid == pid, 
                                           c("mom","pop")]))})
    siblings = ko(KidsOf = KidsOf, p = parents)
    
    #Making sure to remove ego
    siblings = lapply(seq_along(siblings), function(i) siblings[[i]][siblings[[i]] %ni% pid[[i]]])
    
    res$niecesnephews = ko(KidsOf = KidsOf, p = siblings)
  }
  
  ## Grandchildren, grand-nieces, grand-nephews
  #Grandchildren
  
  if (kintype %in% "gchildren"){
    children = ko(KidsOf = KidsOf, p = pid)
    res$gchildren = ko(KidsOf = KidsOf, p = children)
  }
  
  #Grand-nieces and grand-nephews
  if (kintype %in% "gniecesnephews"){
    parents=lapply(pid,
                   function(pid){
                     as.vector(unlist(opop[opop$pid == pid, 
                                           c("mom","pop")]))})
    siblings = ko(KidsOf = KidsOf, p = parents)
    
    #Making sure to remove ego
    siblings = lapply(seq_along(siblings), function(i) siblings[[i]][siblings[[i]] %ni% pid[[i]]])
    
    niecesnephews = ko(KidsOf = KidsOf, p = siblings)
    res$gniecesnephews = ko(KidsOf = KidsOf, p = niecesnephews)
  }
  
  ## Great-grand children
  if (kintype %in% "ggchildren"){
    children = ko(KidsOf = KidsOf, p = pid)
    gchildren = ko(KidsOf = KidsOf, p = children)
    res$ggchildren = ko(KidsOf = KidsOf, p = gchildren)
  }
  
  ###Adding more distant relatives
  
  #Need to go back
  #First cousins, once removed: children of grand-uncles/aunts and also children of first cousins
  if (kintype %in% "firstcousins1rm"){
    #Children of grand-uncles and aunts
    g0=lapply(pid,
              function(pid){
                as.vector(unlist(opop[opop$pid == pid ,
                                      c("MMM","MMF","MFM","MFF",
                                        "FMM","FMF","FFM","FFF")]))})
    g1 = ko(KidsOf = KidsOf, p = g0)
    gparents=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                            c("MM","MF","FM","FF")]))})
    gunclesaunts = lapply(seq_along(g1), 
                          function(i) g1[[i]][g1[[i]] %ni% gparents[[i]]])
    childrengunclesaunts = ko(KidsOf = KidsOf, p = gunclesaunts)
    
    #Children of first cousins
    g2=ko(KidsOf = KidsOf, p = gparents)
    parents=lapply(pid,
                   function(pid){
                     as.vector(unlist(opop[opop$pid == pid, 
                                           c("mom","pop")]))})
    
    unclesaunts = lapply(seq_along(g2), function(i) g2[[i]][g2[[i]] %ni% parents[[i]]])
    firstcousins = ko(KidsOf = KidsOf, p = unclesaunts)
    
    childrenfirstcousins = ko(KidsOf = KidsOf, p = firstcousins)
    
    #Combining children of great uncles and aunts and of first cousins
    res$firstcousins1rm = mapply(c, childrengunclesaunts, childrenfirstcousins, SIMPLIFY=F)
  }
  
  #I removed first cousins twice removed: grandchildren of first cousins and children of great-granduncle/aunt
  #This is incomplete because we do not pick up great-great grandparents
  #I don't think this is likely necessary
  #if (kintype %in% "firstcousins2rm") {
  
  # gparents=lapply(pid,
  #                  function(pid){
  #                   as.vector(unlist(opop[opop$pid == pid,
  #                                        c("MM","MF","FM","FF")]))})
  #g2=ko(gparents)
  #parents=lapply(pid,
  #             function(pid){
  #                as.vector(unlist(opop[opop$pid == pid, 
  #                                    c("mom","pop")]))})
  
  #unclesaunts = lapply(seq_along(g2), function(i) g2[[i]][g2[[i]] %ni% parents[[i]]])
  #firstcousins = ko(unclesaunts)
  
  #childrenfirstcousins = ko(firstcousins)  
  
  #res$firstcousins2rm = ko(childrenfirstcousins)
  #}
  
  #Second cousins: grandchildren of grand-uncles/aunts
  if (kintype %in% "secondcousins") {
    g0=lapply(pid,
              function(pid){
                as.vector(unlist(opop[opop$pid == pid ,
                                      c("MMM","MMF","MFM","MFF",
                                        "FMM","FMF","FFM","FFF")]))})
    g1 = ko(KidsOf = KidsOf, p = g0)
    gparents=lapply(pid,
                    function(pid){
                      as.vector(unlist(opop[opop$pid == pid,
                                            c("MM","MF","FM","FF")]))})
    gunclesaunts = lapply(seq_along(g1), 
                          function(i) g1[[i]][g1[[i]] %ni% gparents[[i]]])
    childrengunclesaunts = ko(KidsOf = KidsOf, p = gunclesaunts)
    
    res$secondcousins = ko(KidsOf = KidsOf, p = childrengunclesaunts)
  }
  
  #I removed second cousins, once removed
  #We don't get great-granduncles/aunts so this is hard to calculate
  #if (kintype %in% "secondcousins1rm") {
  # g0=lapply(pid,
  #      function(pid){
  #         as.vector(unlist(opop[opop$pid == pid ,
  #                                c("MMM","MMF","MFM","MFF",
  #                                   "FMM","FMF","FFM","FFF")]))})
  #g1 = ko(g0)
  #gparents=lapply(pid,
  #            function(pid){
  #               as.vector(unlist(opop[opop$pid == pid,
  #                                       c("MM","MF","FM","FF")]))})
  #gunclesaunts = lapply(seq_along(g1), 
  #                    function(i) g1[[i]][g1[[i]] %ni% gparents[[i]]])
  # childrengunclesaunts = ko(gunclesaunts)
  
  #  secondcousins = ko(childrengunclesaunts)
  #res$secondcousins1rm = ko(secondcousins)
  #}
  
  #I removed second cousins, twice removed
  #This is easier to calculate but I'm not sure it's necessary
  #if (kintype %in% "secondcousins2rm") {
  #  g0=lapply(pid,
  #           function(pid){
  #            as.vector(unlist(opop[opop$pid == pid ,
  #                                  c("MMM","MMF","MFM","MFF",
  #                                    "FMM","FMF","FFM","FFF")]))})
  #g1 = ko(g0)
  #gparents=lapply(pid,
  #               function(pid){
  #                as.vector(unlist(opop[opop$pid == pid,
  #                                     c("MM","MF","FM","FF")]))})
  #gunclesaunts = lapply(seq_along(g1), 
  #                     function(i) g1[[i]][g1[[i]] %ni% gparents[[i]]])
  #childrengunclesaunts = ko(gunclesaunts)
  
  #secondcousins = ko(childrengunclesaunts)
  #secondcousins1rm = ko(secondcousins)
  #res$secondcousins2rm = ko(secondcousins1rm)
  #}
  
  #--In-laws
  
  #Spouse
  if (kintype %in% "spouse") {
    res$spouse <- so(opop = opop, p = pid)
  }
  
  #Parents-in-law
  if (kintype %in% "parentsofspouse") {
    spouse <- so(opop = opop, p = pid)
    res$parentsofspouse=lapply(spouse,
                               function(x){
                                 unlist(opop[opop$pid %in% x,
                                             c("mom","pop")])})
  }
  
  #Brothers and sister in laws = siblings of spouse/spouse of siblings
  #Spousesofsiblings
  
  if (kintype %in% "brothersisterinlaw") {
    
    #Spouse of siblings
    parents=lapply(pid,
                   function(pid){
                     as.vector(unlist(opop[opop$pid == pid, 
                                           c("mom","pop")]))})
    siblings = ko(KidsOf = KidsOf, p = parents)
    
    #Making sure to remove ego
    siblings = lapply(seq_along(siblings), function(i) siblings[[i]][siblings[[i]] %ni% pid[[i]]])
    
    spouseofsiblings <- so(opop = opop, p = siblings)
    
    #Siblings of spouse
    spouse <- so(opop = opop, p = pid)
    parentsofspouse=lapply(spouse,
                           function(x){
                             unlist(opop[opop$pid %in% x,
                                         c("mom","pop")])})
    siblingsofspouse <- ko(KidsOf = KidsOf, p= parentsofspouse)
    #Removing spouse from list of siblings of spouse
    siblingsofspouse <- lapply(seq_along(siblingsofspouse), 
                               function(i) siblingsofspouse[[i]][siblingsofspouse[[i]] %ni% spouse[[i]]])
    
    #Combining spouse of siblings and siblings of spouse  
    res$brothersisterinlaw <- mapply(c, spouseofsiblings, siblingsofspouse, SIMPLIFY=F)
  }
  
  #--Data cleaning
  #Replacing NULLs and integer(0) with NA
  for (i in 1:length(names(res))) {
    res[[i]][sapply(res[[i]], function(x) length(x)==0)] <- NA #removing NULLs and integer(0) with NA
  }
  
  return(res)
}


#~~~~~~~Directory Stem level (Level 1)
getAllEstimates <- function(dir_stem) {
  #~~~~~~~~~~Level 2: Looping over countries
  
  #Simulations are in this folder
  #This function presumes covid simulations are stored in Mallika Snyder's 90 days folder, 
  #and counterfactual simulations in Diego Alburez-Gutierrez's 90days folder
  
  #The simulations are in these folders
  folders <- findFolders(dir_stem = dir_stem)
  
  #Now loop over the folders
  all_country_tibbles <- lapply(folders, function(x) 
    getCountryEstimates(UNrates2019 = paste0("~/covid_simulation/","UNrates2019", "/"), 
                        feb2020 = 3241,
                        num_each = 5,
                        folder = x))
  
  #Bind tibbles
  all_country_data <- bind_rows(all_country_tibbles) %>%
    mutate(scenario = dir_stem)
  
  print(paste0("Estimates generated:", dir_stem))
  
  return(all_country_data)
  
}

#~~~~~~~Country (folder) level (Level 2)
getCountryEstimates <- function(UNrates2019 = paste0("~/covid_simulation/","UNrates2019", "/"), 
                                feb2020 = 3241,
                                num_each = 5,
                                folder) {
  #Find the country name
  countryname <- strsplit(folder, "/")
  countryname <- gsub(":Medium.sup", "", unlist(countryname))
  countryname <- countryname[length(countryname)]
  
  #Find number of months for which a country has Covid-adjusted mortality
  nmonths <- length(grep(list.files(paste0(UNrates2019, countryname, "/Mort/")), 
                         pattern = "Month", invert = F, value = T)) - 2
  
  #Month after Covid-adjusted mortality
  postcovid2020 <- feb2020 + nmonths + 1
  
  #Find the numbers of the simulations, and subset to the first num_each number of them
  sim.nums <- grep(list.files(path = folder), 
                   pattern='.opop$', invert=TRUE, value=TRUE)[1:num_each]
  
  #Then generate estimates by country/scenario combination
  all_sim_tibbles <- lapply(sim.nums, function(x) getSimEstimates(folder = folder,
                                                                  feb2020 = feb2020,
                                                                  postcovid2020 = postcovid2020, 
                                                                  sim.name = x))
  
  sim_data <- bind_rows(all_sim_tibbles) %>%
    mutate(country = countryname)
  
  print(paste0("Estimates generated:", countryname))
  
  return(sim_data)
}


#~~~~~~~~Simulations level (looping over simulations) (Level 3)

getSimEstimates <- function(folder = folder,
                            feb2020 = feb2020, postcovid2020 = postcovid2020,
                            sim.name){
  
  print(paste0("Starting:", sim.name))
  
  #Identify the simulation
  sim.id <- sim.name
  
  #Remove the objects
  #Now load the data
  sims <- findSimObject(findPath(folder = folder, name = sim.id))
  #Same as load(file = findPath(sim.id))
  
  # Function parameters for asYr()
  FinalSimYear <- 2035 #Are these correct? #The first month is 0, the last month is 3240
  #It makes sense we have one extra month in 2035, since our first month (month 1) is actually a February
  endmo <- 3420
  
  #Adding opop to omar
  opop <- add2opop2(FinalSimYear = FinalSimYear, endmo = endmo, 
                    opop = sims$opop, omar = sims$omar)
  
  #Filtering out columns we don't need
  opop <- opop[, -which(names(opop) %in% 
                          c("group", "nev", "nesibm", "nesibp", 
                            "lborn", "marid", "mstat", "fmult", "aod"))]
  
  #Filtering to only individuals alive in February 2020: this is month 3241
  #I found that by taking the second unique value of dates of birth for individuals in the 2020 birth cohort
  opop <- opop[opop$dob <= feb2020,] #do we want to filter on dod (& opop$dod > feb2020)? Put minimum limit on dob as well
  
  #Filtering cohorts pre-1800: we may want to reconsider this
  opop <- opop[opop$cohort >= 1800,]
  
  #Person ID vector for all egos
  #Filtering so we get kin for only individuals who survive the Covid period
  pid <- opop$pid[opop$dod > postcovid2020]
  
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
  
  #Kin relationship types to loop over
  all.kin.types <- c("gparents", "gunclesaunts",
                     "parents", "unclesaunts",
                     "siblings", "firstcousins", "children",
                     "spouse", "parentsofspouse") #removed g0 and brothersisterinlaw
  
  
  #~~~~~~~~~~~~Level 4: Looping over kin relationships
  #Run the analysis code on all kin relationships
  #We parallelize at this level (will not work in Windows)
  
  all_tibbles <- mclapply(all.kin.types, function(x) 
    getKinCounts(opop = opop, KidsOf = KidsOf, ego.dod = ego.dod,
                 ego.cohort = ego.cohort, ego.female = ego.female, 
                 pid = pid, feb2020 = feb2020, postcovid2020 = postcovid2020, kin.name = x),
    mc.cores = 5)
  
  #Original code without parallelization:
  #all_tibbles <- lapply(all.kin.types, function(x) 
  # getKinCounts(opop = opop, KidsOf = KidsOf, ego.dod = ego.dod,
  #             ego.cohort = ego.cohort, ego.female = ego.female, 
  #            pid = pid, feb2020 = feb2020, postcovid2020 = postcovid2020, kin.name = x))
  
  #Now bind these rows and add some id variables
  data <- dplyr::bind_rows(all_tibbles) %>%
    mutate(sim.id = sim.id)
  
  #Save intermediate output
  save(data, file = paste0("~/covid_simulation/Data/sim_results/data", sim.id, ".RData"))
  
  print(paste0("Estimates generated:", sim.id))
  
  return(data)
  
}


#~~~~~~~~Analysis code for generating kin count tibbles (Level 4) #parallelize further down
getKinCounts <- function(opop = opop, KidsOf = KidsOf,
                         ego.dod = ego.dod,
                         ego.cohort = ego.cohort, ego.female = ego.female, 
                         pid = pid, feb2020 = feb2020, postcovid2020 = postcovid2020,
                         kin.name){
  
  #Find the kin of ego for a particular kin relationship
  kin <- getKinEach(opop = opop, KidsOf = KidsOf, pid = pid, kintype = kin.name)
  
  #Vectorize this and add attributes
  kin2 <- lapply(kin, function(x) 
    getVectorizedForm(pid = pid, ego.cohort = ego.cohort, ego.dod = ego.dod, 
                      ego.female = ego.female, opop = opop, 
                      feb2020 = feb2020, postcovid2020 = postcovid2020, object = x))
  
  #Groups: birth cohorts and sexes
  cohorts <- unique(attr(kin2[[1]], "ego.cohort")) #we can index by 1 because there will be only one list
  sexes <- unique(attr(kin2[[1]], "ego.female"))
  
  #Find number of kin alive for each category: pre-Covid
  rez <- attr(kin2[[1]], "still.alive.feb2020")
  names(rez) <- attr(kin2[[1]], "names") #PIDs
  
  # DAG START ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # I have the feeling that these functions can be sped up by removing some of the lapplys
  # or combining them. 
  # I had a first go at it below but I don't think it really works...
  
  # START OLD CODE -------------------------------------------
  #Number of kin by ego
  num_pre <- unlist(lapply(unique(names(rez)), 
                           function(x) sum(rez[which(names(rez) == x)], na.rm = T)))
  names(num_pre) <- paste0(ego.cohort, "f", ego.female)  #Name these categories
  #Number of kin by cohort and sex
  num_pre_cohort_sex <- unlist(lapply(unique(names(num_pre)), 
                                      function(x) 
                                        mean(num_pre[which(names(num_pre) == x)], na.rm = T))) 
  #Calculate sd
  sd_pre_cohort_sex <- unlist(lapply(unique(names(num_pre)), 
                                     function(x) 
                                       sd(num_pre[which(names(num_pre) == x)], na.rm = T))) 
  names(num_pre_cohort_sex) <- unique(names(num_pre)) #Name these categories
  # END OLD CODE ---------------------------
  
  # START NEW POTENTIAL CODE ++++++++++++++++++++++++++++
  # # This is how I interpreted your code, but it gives me '8' for every element
  # # so maybe there is something wrong with my code... in any case, something like
  # # this could help us get rid of that initial lapply, which takes  long time
  # num_pre <- unlist(table(names(rez)))
  # 
  # # This is meant to run only one loop to get the mean and sd at the same time
  # # The output is different from your original output, so some re-weriting might
  # # be necessary
  # mean_sd <- lapply(unique(names(num_pre)), 
  #                   function(x) {
  #                     matches <- num_pre[which(names(num_pre) == x)]
  #                     out <- list(
  #                       mean = mean(matches, na.rm = T)
  #                       , sd = sd(matches, na.rm = T)
  #                     )
  #                   })
  # END NEW POTENTIAL CODE ++++++++++++++++++++++++++++
  # DAG END ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  #Find number of kin alive for each category: post-Covid
  rez <- attr(kin2[[1]], "still.alive.postcovid2020")
  names(rez) <- attr(kin2[[1]], "names") #PIDs
  
  #Number of kin by PID
  num_post <- unlist(lapply(unique(names(rez)), 
                            function(x) sum(rez[which(names(rez) == x)], na.rm = T)))
  names(num_post) <- paste0(ego.cohort, "f", ego.female) #Naming these categories
  #Number of kin by cohort and sex
  num_post_cohort_sex <- unlist(lapply(unique(names(num_post)), 
                                       function(x) 
                                         mean(num_post[which(names(num_post) == x)], na.rm = T)))
  #Add sd values
  sd_post_cohort_sex <- unlist(lapply(unique(names(num_post)), 
                                      function(x) 
                                        sd(num_post[which(names(num_post) == x)], na.rm = T)))
  names(num_post_cohort_sex) <- unique(names(num_post)) #Naming these categories
  
  #Tibble for precovid period
  tibble_pre <- as_tibble(names(num_pre_cohort_sex)) %>%
    add_column(num_kin = unname(num_pre_cohort_sex), sd = sd_pre_cohort_sex) %>%
    mutate(time = "precovid", kin.type = kin.name) %>%
    separate(value, sep="f", into = c("cohort", "sex"))
  
  #Tibble for postcovid period
  tibble_post <- as_tibble(names(num_post_cohort_sex)) %>%
    add_column(num_kin = unname(num_post_cohort_sex), sd = sd_post_cohort_sex) %>%
    mutate(time = "postcovid", kin.type = kin.name) %>%
    separate(value, sep="f", into = c("cohort", "sex"))
  
  #Bind all rows
  tibble <- dplyr::bind_rows(tibble_pre, tibble_post)
  
  return(tibble)
}



#~~~~~~~~~New analysis functions
#Now calculate kin counts for the two methods
getKinCountsNew <- function(all_cat = all_cat, kin, kin.type, pid = pid, 
                            ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                            ego.female = ego.female, opop = opop, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020, 
                            sim.id = sim.id, country = country, scenario = scenario) {
  
  #Vectorize kin counts
  kin2 <- getVectorizedForm(pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                            ego.female = ego.female, opop = opop, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020,
                            object = kin)
  
  #Now we want to run two different processes on the data
  #The first is to calculate the bereavement multiplier: average number of kin for individuals who die during the Covid period
  
  kin_bm <- bind_rows(lapply(all_cat, function(x) {
    #All egos who died
    ego.names <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                           attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                           data.table::between(attr(kin2, "ego.dod"), 
                                                               feb2020, postcovid2020, incbounds = F))]))
    
    #Kin for those who died
    table <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), 
                          levels = ego.names))
    
    output <- list(mean = mean(table, na.rm = T), sd = sd(table, na.rm = T), n_deaths = length(table),
                   category = x, kintype = kin.type, sim.id = sim.id, country = country, scenario = scenario)
    
    return(output)
  }))
  
  #The second is to find the average number of kin before and after for those who survive: it will return a tibble
  kin_bb <- bind_rows(lapply(all_cat, function(x) {
    #Find names of survivors
    ego.names.bb <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                              attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                              attr(kin2, "ego.dod") >= postcovid2020)]))
    
    #Find kin counts
    num_pre <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), 
                            levels = ego.names.bb))
    
    num_post <- table(factor(names(kin2[which(attr(kin2, "still.alive.postcovid2020") == TRUE)]), 
                             levels = ego.names.bb))
    
    #length(n_pre) = length(n_post)
    output <- list(mean_pre = mean(num_pre, na.rm = T), sd_pre = sd(num_pre, na.rm = T), n_survivors = length(num_pre), 
                   mean_post = mean(num_post, na.rm = T), sd_post = sd(num_post, na.rm = T),
                   category = x, kintype = kin.type, sim.id = sim.id, country = country, scenario = scenario)
    return(output)
    
  }))
  
  kin_both <- list(kin_bm = kin_bm, kin_bb = kin_bb)
  
  return(kin_both)
}

#New function for female kin
getKinCountsFemale <- function(all_cat = all_cat, kin, kin.type, pid = pid, 
                               ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                               ego.female = ego.female, opop = opop, 
                               feb2020 = feb2020, postcovid2020 = postcovid2020, 
                               sim.id = sim.id, country = country, scenario = scenario) {
  
  #Vectorize kin counts
  kin2 <- getVectorizedForm(pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                            ego.female = ego.female, opop = opop, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020,
                            object = kin)
  
  
  #Find the average number of kin before and after for those who survive: it will return a tibble
  kin_female <- bind_rows(lapply(all_cat, function(x) {
    #Find names of survivors
    ego.names.bb <- unique(names(kin2[which(attr(kin2, "ego.agefeb2020") == unlist(strsplit(x, split = "f"))[1] &
                                              attr(kin2, "ego.female") == unlist(strsplit(x, split = "f"))[2] &
                                              attr(kin2, "ego.dod") >= postcovid2020)]))
    
    #Find kin counts
    num_pre <- table(factor(names(kin2[which(attr(kin2, "still.alive.feb2020") == TRUE)]), 
                            levels = ego.names.bb))
    
    num_post <- table(factor(names(kin2[which(attr(kin2, "still.alive.postcovid2020") == TRUE)]), 
                             levels = ego.names.bb))
    
    #length(n_pre) = length(n_post)
    output <- list(mean_pre = mean(num_pre, na.rm = T), sd_pre = sd(num_pre, na.rm = T), n_survivors = length(num_pre), 
                   mean_post = mean(num_post, na.rm = T), sd_post = sd(num_post, na.rm = T),
                   category = x, kintype = kin.type, sim.id = sim.id, country = country, scenario = scenario)
    return(output)
    
  }))
  
  
  return(kin_female)
}


getEstimatesNew <- function(sim.id, country, scenario, 
                            feb2020 = feb2020, postcovid2020 = postcovid2020) {
  #First level needs
  folder <- paste0("~/90days/", scenario, "RESULTS/", "SocCovid-mallikasnyder/", country, ":Medium.sup")
  
  print(paste0("Starting:", sim.id))
  
  #Remove the objects
  #Now load the data
  sims <- findSimObject(findPath(folder = folder, name = sim.id))
  #Same as load(file = findPath(sim.id))
  
  # Function parameters for asYr()
  FinalSimYear <- 2035 #Are these correct? #The first month is 0, the last month is 3240
  #It makes sense we have one extra month in 2035, since our first month (month 1) is actually a February
  endmo <- 3420
  
  #Parameters for the add2opop2 and death rate functions
  age_br_months <- c(0, 14, 29, 44, 64, Inf)*12
  age_labs <- c("0-14", "15-29", "30-44", "45-64", "65+")
  
  
  #Adding opop to omar
  opop <- add2opop2(FinalSimYear = FinalSimYear, endmo = endmo, 
                    feb2020 = feb2020, age_br_months = age_br_months, age_labs = age_labs,
                    opop = sims$opop, omar = sims$omar)
  
  #Filtering out columns we don't need
  opop <- opop[, -which(names(opop) %in% 
                          c("group", "nev", "nesibm", "nesibp", 
                            "lborn", "marid", "mstat", "fmult"))]
  
  #Filtering to only individuals alive in February 2020: this is month 3241
  #I found that by taking the second unique value of dates of birth for individuals in the 2020 birth cohort
  opop <- opop[opop$dob <= feb2020,] #do we want to filter on dod (& opop$dod > feb2020)? Put minimum limit on dob as well
  
  #Filtering cohorts pre-1800: we may want to reconsider this
  opop <- opop[opop$cohort >= 1800,]
  
  #Calculate death rates
  death_rates <- save_mortality_rates_first_wave_to_disk(
    df = opop
    , sim.id = sim.id
    , country = country
    , scenario = scenario
    , get_death_rate = T
    , get_asmr = T
    , age_br_months = age_br_months
    , age_labs = age_labs
    , FinalSimYear = FinalSimYear
    , endmo = endmo
    , feb2020 = feb2020
    , postcovid2020 = postcovid2020
    , path_out = "~/covid_simulation/Data/death_rates/"
  )
  
  #Person ID vector for all egos
  #Filtering so we get kin for only those individuals who either survive the Covid period or who die during it
  pid <- opop$pid[opop$dod > feb2020]
  
  #Vector with age intervals of egos
  ego.agefeb2020 <- opop$agefeb2020_gr[opop$pid %in% pid]
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
  
  #Find kin of these PIDs
  kin <- getKinTogether(opop = opop, KidsOf = KidsOf, pid = pid)
  
  #All categories: age and sex
  all_cat <- unique(paste0(ego.agefeb2020, "f", ego.female))
  
  #Run the analysis code
  temp_data <- lapply(1:length(kin), function(x) getKinCountsNew(all_cat = all_cat, 
                                                                 kin = kin[[x]], kin.type = names(kin[x]), 
                                                                 pid = pid, ego.agefeb2020 = ego.agefeb2020, ego.dod = ego.dod, 
                                                                 ego.female = ego.female, opop = opop, 
                                                                 feb2020 = feb2020, postcovid2020 = postcovid2020, 
                                                                 sim.id = sim.id, country = country, scenario = scenario))
  
  #Bind into tibbles
  flat_data <- do.call(c, temp_data)
  data_bm <- bind_rows(flat_data[which(names(flat_data) %in% "kin_bm")])
  data_bb <- bind_rows(flat_data[which(names(flat_data) %in% "kin_bb")])
  
  #Add death_rates
  data <- list(kin_bm = data_bm, kin_bb = data_bb, 
               death_rates = death_rates)
  
  #Save intermediate output
  save(data, file = paste0("~/covid_simulation/Data/sim_results_paa/data", sim.id, ".RData"))
  
  print(paste0("Estimates generated:", sim.id))
  
}


save_mortality_rates_first_wave_to_disk <- function(df, sim.id, country, scenario,
                                                    get_death_rate = T, 
                                                    get_asmr = T, age_br_months, 
                                                    age_labs, FinalSimYear, 
                                                    endmo, feb2020, postcovid2020, path_out){
  
  # From socsim opop file
  # Do all estimates in months!
  # Feb 2020 is feb2020 = 3241
  # feb2020 should exist already 
  
  # Returns no value!
  
  # This estimates the mortality rate for the entire year
  # month_range <- c(feb2020 - 1, feb2020 + 10)
  # This estimates the mortality rate for Feb - to the end of the pandemic
  # assumed to be August 2020
  month_range_low <-  feb2020 #feb2020 - 1
  month_range_high <- postcovid2020    #feb2020 + 6
  
  mid_period_month <- trunc(month_range_low + diff(c(month_range_low, month_range_high))/2)
  
  # A dataframe of people alive at start of period
  alive_at_start <- 
    df %>% 
    mutate(
      dod2 = ifelse(dod == 0, endmo, dod)
      , age_death_months = dod2-dob #ifelse(dod == 0,NA,dod-dob)
      , age_mid_period_months = mid_period_month-dob #ifelse(dod == 0,NA,mid_period_month-dob)
    ) %>% 
    select(dob, dod2, fem, age_death_months, age_mid_period_months) %>% 
    mutate(
      age_death_gr = cut(age_death_months, age_br_months, age_labs, include.lowest = T)
      , age_mid_period_gr = cut(age_mid_period_months, age_br_months, age_labs, include.lowest = T)
    ) %>% 
    dplyr::filter(dob <= month_range_low & dod2 > month_range_low) #removed: dod2 = month_range_low
  
  # DEATH RATE
  if(get_death_rate){
    
    deaths_vector <- alive_at_start$dod2
    
    mid_period_pop <- sum(deaths_vector >= mid_period_month)
    
    # This is already a vector born before start of period
    # and who died beore of period, so just need
    # to filter to keep those that died in the given interval
    deaths_period <- sum(deaths_vector <= month_range_high)
    
    death_rate <- deaths_period/mid_period_pop * 1000
    
    
    # Export 
    write(x = death_rate, paste0(path_out, "death_rates_", sim.id, ".txt"))
    
  }
  
  # Get Age-specific mortality rate for given age groups
  if(get_asmr){
    
    # Numerator - death counts by age and sex
    
    numerator <- 
      alive_at_start %>% 
      # keep only those that died before end of period
      filter(dod2 < month_range_high) %>%  #removed =
      dplyr::count(fem, age = age_death_gr) %>% 
      tidyr::complete(fem, age, fill = list(n = 0)) %>% 
      arrange(fem, age)
    
    # Denominator 
    
    # Currently, this is the age-sex distribution of people who were alive at the start of the period
    # considering their age at the middle of the period. 
    # Now, it only considers the year of death, meaning that the poulation includes all those who were alive 
    # at the start of a given year and did not die that year.
    # This is consistent with HMD's definition of "alive on January 1st" but could still be corrected
    # to account for "the timing of deaths during the interval and variation in cohort's birthdays by month"
    
    denominator <-
      alive_at_start %>%
      # Keep people that survived the entire period
      #filter(dod2 >= month_range_high) %>% 
      dplyr::count(fem, age = age_mid_period_gr) %>% 
      tidyr::complete(fem, age, fill = list(n = 0)) %>% 
      arrange(fem, age)
    
    # Rate
    asmr <- 
      numerator %>% 
      left_join(denominator, by = c("fem", "age"), suffix = c("_num", "_den")) %>% 
      mutate(
        value = n_num / n_den
        , value = ifelse(is.na(value), 0, value) * 1000
        , sim.id = sim.id
        , age = as.character(age)
        , fem = as.character(fem)
      ) 
    
    write.csv(x = asmr, paste0(path_out, "asmr_", sim.id, ".csv"), row.names = F)
  }
  
  print("Death rates saved to disk: death_rates object now in workspace")
  
  death_rates <- asmr %>%
    add_row(fem = "all_sum", age = "all_sum", 
            n_num = sum(asmr$n_num), n_den = sum(asmr$n_den), value = 1000*(n_num/n_den),
            sim.id = sim.id) %>%
    add_row(value = death_rate, fem = "all_mid", age = "all_mid", 
            n_num = deaths_period, n_den = mid_period_pop, sim.id = sim.id) %>%
    mutate(country = country, scenario = scenario)
  
  return(death_rates)
}