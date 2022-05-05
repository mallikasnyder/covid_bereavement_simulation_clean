#Adapted from Carl Mason's code
#Currently I'm identifying the kin relationships and placing each relationship thus derived in a nested list, res
#See this chart for definitions: https://www.familysearch.org/blog/en/cousin-chart/

#---Loading data and writing functions

#Identifying opop and omar
opop<-sims$opop
omar<-sims$omar

#Adding opop to omar
opop <- add2opop(sims$opop,omar)

#Filter
#opop <- opop %>%
#  filter(between(cohort, 1800, 2025))

#FUNCTION for finding children
kidsOf<-with(opop,{c(
  tapply(pid,mom,c),
  tapply(pid,pop,c)
)})

#Using indexes saves space
#We have to run both of these since there are two 0s
kidsOf["0"]<-NULL;  kidsOf["0"]<-NULL
KidsOf<-list()
KidsOf[as.numeric(names(kidsOf))]<-kidsOf

ko <- function(p){
  lapply(p,function(x){unique(as.vector(unlist(KidsOf[x])))})
}

### FUNCTION for finding ego's spouse
so <- function(q){
  lapply(q,function(q){as.vector(unlist(opop[opop$pid == q,
                                   c("spouse")]))})
}
#Try this with %in%

#Defining a test dataset
pid<-opop[opop$cohort==2015,"pid"]

#Creating a list to hold these various kin relationships
res<-list(
  ## ego is in g3 
  pid)


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
                     unlist(opop[opop$pid == pid,
                                 c("mom","pop")])})
#Uncles and Aunts
res$unclesaunts = lapply(seq_along(res$g2), function(i) res$g2[[i]][res$g2[[i]] %ni% res$parents[[i]]])

#---The family tree at ego’s generation (g3)

##Cousins and Siblings
res$g3 = ko(res$g2)

#Siblings
res$siblings = ko(res$parents)
#making sure to remove ego
res$siblings = lapply(seq_along(res$siblings), function(i) res$siblings[[i]][res$siblings[[i]] %ni% pid[[i]]])

#First cousins
res$firstcousins = ko(res$unclesaunts)

#---The family tree below ego’s generation

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

#-----Adding in-laws

#Spouse
res$spouse <- so(pid)

#Parents-in-law
res$parentsofspouse=lapply(res$spouse,
                           function(x){
                             unlist(opop[opop$pid %in% x,
                                         c("mom","pop")])})
#I get a warning but it works ("longer object length is not a multiple of shorter object length")
#Could also use match() or %in% (allows you to compare vectors of different lengths)

#Brothers and sister in laws = siblings of spouse/spouse of siblings
#Spousesofsiblings

res$spouseofsiblings <- so(res$siblings)
res$siblingsofspouse <- ko(res$parentsofspouse)
res$siblingsofspouse <- lapply(seq_along(res$siblingsofspouse), 
                               function(i) res$siblingsofspouse[[i]][res$siblingsofspouse[[i]] %ni% res$spouse[[i]]])
res$brothersisterinlaw <- mapply(c, res$spouseofsiblings, res$siblingsofspouse, SIMPLIFY=F)

#---Data cleaning
#Replacing NULLs and integer(0) with NA
#I'm not sure if this is necessary but I do it here in case it is helpful later.

list_of_kin <- names(res)

for (i in 1:length(list_of_kin)) {
  res[[i]][sapply(res[[i]], function(x) length(x)==0)] <- NA
  res[[i]] <- sapply(res[[i]], function(x) unname(x))
}

#res$siblingsofspouse[sapply(res$siblingsofspouse, function(x) length(x)==0)] <- NA

#---Adding step-kin: 
#Our current definitions of siblings will not count step-kin as siblings
#this is an optional step that we may wish to add later--the tricky part is figuring out dates if unions

#From those we would need step-parents, step siblings and half siblings

#Step-parents

#Step-children
#res$stepchildren <- ko(res$spouse)
#res$stepchildren <- lapply(seq_along(res$stepchildren), 
                          # function(i) res$stepchildren[[i]][res$stepchildren[[i]] %ni% res$children[[i]]])
#We have very few instances of this occurring
#Step siblings=children of parent's spouse that share neither parent with you
#Half siblings= children of parent's spouse that share one, but not both parents with you

#---Extra code

#Sample code to check
###Here is a way to check one of these quantities
#check <- mapply(c, res$gparents, res$gunclesaunts, SIMPLIFY=F)
#check <- lapply(seq_along(check), function(a) unname(check[[a]]))
#check <- lapply(seq_along(check), function(x) sort(check[[x]]))
#checkg1 <- lapply(seq_along(res$g1), function(x) sort(res$g1[[x]]))
#sum(compare.list(check, checkg1))
#This works!

#Considering vectorization (this doesn't work)

#Old code
#FUNCTION for finding children
kidsOf<-with(opop,{c(
  tapply(pid,mom,c),
  tapply(pid,pop,c)
)})

#Using indexes saves space
#We have to run both of these since there are two 0s
kidsOf["0"]<-NULL;  kidsOf["0"]<-NULL
KidsOf<-list()
KidsOf[as.numeric(names(kidsOf))]<-kidsOf

###Final function
ko <- function(p){
  lapply(p,function(x){unique(as.vector(unlist(KidsOf[x])))})
}

ko2 <- function(p) {
  kids <- lapply(p,function(x){unique(as.vector(unlist(KidsOf[x])))})
  return(kids)
}



####Final version of function
ko <- function(p) {
  kids <- lapply(p,function(x){unique(as.vector(unlist(KidsOf[x])))})
  kids[which((length(kids) == 0)|kids == "NULL")] <- NA
  kids2 <- unlist(kids)
  names(kids2) <- rep(p, unlist(lapply(kids, length))) #names are the person IDs they are associated with
  attr(kids2, "aod") <- rep(ego.aod, unlist(lapply(kids, length))) #adding an attribute for PID age of death
  return(kids2)
}

### FUNCTION for finding great grandparents (who were already found)
ggparentso <- function(p){
  g0 <- as.vector(unlist(opop[opop$pid %in% p,
                              c("MMM","MMF","MFM","MFF",
                                "FMM","FMF","FFM","FFF")))
  g0[which((length(g0) == 0)|g0 == "NULL")] <- NA
  g02 <- unlist(g0)
  names(g02) <- rep(p, unlist(lapply(g0, length))) #names are the person IDs they are associated with
  attr(g02, "aod") <- rep(ego.aod, unlist(lapply(g0, length))) #adding an attribute for PID age of death
  return(g02)
}

### FUNCTION for finding grandparents (who were already found)

### FUNCTION for finding parents of an individual

### FUNCTION for finding spouses (vectorized)
so <- function(p){
  spouse <- opop$spouse[which(opop$pid %in% p)]
  names(spouse) <- p
  return(spouse)
}

