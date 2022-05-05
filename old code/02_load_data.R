# Finding pathways

#Find the user
USER <-system("whoami",intern=TRUE) 
#We need to figure out the directory for this: 
#Should this just point to "mallikasnyder"?

#The COVID scenario simulations are in the covidRESULTS directory, the comparison are in otherRESULTS
all_dir_stems <- c("covid", "other")

# Function parameters --------------
FinalSimYear <- 2100
endmo <- 4200

#Specify number of cores
#Find operating system
os <- Sys.info()['sysname']
numCores <- switch(os, Windows = 25, Linux = 10)

#Old code
#Stem <- "~/MPIDR/Covid Bereavement/Data/socsim_sample/Sweden_Medium.sup/20075/SimResults/sims" 
#sfile <- paste(Stem,".Rsave",sep='')
#load(file = sfile)
