#!/usr/bin/env Rscript
###
### Marco Smolla, Manchester University
### 15-05-2014


#########################################
##     #####   ##  ##  ##  ##   ###    ##
##     ##  ##  ##  ##  ### ##   ###    ##
##     #####   ##  ##  ######   ###    ##
##     ## ##   ##  ##  ## ###          ##
##     ##  ##   ####   ##  ##   ###    ##
#########################################

### Set up
# determine the queue number of this process (necessary when saving the results)
### Load world parameters

# BEGIN SLURM Computer Cluster
### Set-up packages
if(Sys.getenv("USER")=="uma"){ # At Slurm my user name is uma, if this is detected, we are executing on Slurm

  syse <- Sys.getenv(c("SLURM_ARRAY_JOB_ID","SLURM_ARRAY_TASK_ID"))
  jobID <- syse[1]
  queue <- syse[2]

  ### load additional packages
  require(methods)
  require(dplyr)

  ### Load methods
  if(file.exists('methods.tar')) untar(tarfile="methods.tar", exdir=".")
  setwd('methods')
  files<-list.files()
  lapply(files, source)
  setwd('../')
  if(file.exists('methods.tar'))	unlink('methods',recursive=TRUE)

} else { #END CONDOR
  ### For local simulations:
  ### Load methods
  setwd('/Users/marco/Documents/Programming/R/learning_sexes/skew_learning_git/methods/') # change this line to current path of methods folder
  l <- lapply(list.files(), source)
  ### set queue to 1
  queue <- 1
  ### load additional packages
  library(parallel)
  library(dplyr)
}


### Defining run routine
run <- function(setup, ceed=NULL){
  # Create and store seed information
  if(is.null(ceed)) ceed <- round(runif(1,0,99999999))
  set.seed(ceed)
  setup$seed <- ceed

  ### Defining the data objects
  patDF <- as.matrix(data.frame(id=0:(setup$nPat), payoff=0, agentsPresent=NA, competitionPayOff=NA))#;patDF #formerly known as world@patch ### Patches in the world
  indDF <- as.matrix(data.frame(id=1:setup$nInd, nRound=1, nExploit=0, innovateProp=NA, atPatch=0, learn=NA, fitness=0, yield=0, yield2=0, S=0, fitness_variance=0, m1=NA,f1=NA))#;indDF ### Single data to be stored for every living individual (row-wise)

  ### Multiple data stored for every individual (column-wise)
  indDF_repertoir <- matrix(NA,nrow=setup$nInd, ncol=(setup$nPat+1)) #nrow is max 100 because all individuals die
  indDF_collectedPayoff <- matrix(NA, ncol=setup$maxAge, nrow=setup$nInd)#columns are rounds, rows are indviduals

  patDF <- changeWorld(patDF=patDF, pat=nrow(patDF), create = T, payoffDis = as.character(setup$payoffDis), mod = '', setup=setup)# ;patDF
  # Set males and females; 1=female, 0=male
  if( round(setup$SexStratProp*setup$nInd)>0 ) indDF[ 1:round(setup$SexStratProp*setup$nInd) ,'S'] <- 1
  # Select columns to set genetic predisposition for individual learning, for male parent 1/2, and female parent 1/2 gene
  genes <- colnames(indDF)%in%c("m1","f1")
  # Choose values from a uniform distribution for this predisposition

  if(setup$strategyProportion==2) indDF[,genes] <- round(runif(min=0,max=1,n=setup$nInd*2),2) # for free innovateProp
  if(setup$strategyProportion!=2) indDF[,genes] <- round(runif(min=0,max=1,n=setup$nInd*2)) # for fixed innovateProp
  # Assign values for females from mothers
	indDF[ indDF[,"S"]==1 ,"innovateProp"] <- indDF[ indDF[,"S"]==1 , "f1"]
  # Assign values for males from fathers
	indDF[ indDF[,"S"]==0 ,"innovateProp"] <- indDF[ indDF[,"S"]==0 , "m1"]


  time <- 0
  allSame <- length(unique(indDF[,"innovateProp"]))==1
  fems <- rep(NA, setup$nRound)
  mems <- rep(NA, setup$nRound)

  while(!allSame){
    time <- time+1
    patDF           <- changeWorld(patDF=patDF, pat=nrow(patDF), create = F, payoffDis = as.character(setup$payoffDis), mod='', setup=setup) #;patDF
    observable 			<- observation(indDF) ;observable
    indDF 					<- whichAction(indDF = indDF, nInd = setup$nInd, indDF_repertoir = indDF_repertoir, patDF=patDF, setup=setup) #;indDF
    indDF 					<- whichLearning(nInd = setup$nInd, indDF = indDF) ;indDF
    indDF_repertoir <- formingMemory(setup = setup, patDF = patDF, indDF = indDF, indDF_repertoir = indDF_repertoir, observable = observable, mod='') #;indDF_repertoir
    patDF           <- individualsAtPatch(patDF = patDF, indDF = indDF, setup=setup) #;patDF
    indDF           <- whichPayoff(setup = setup, patDF = patDF, indDF = indDF, mod = '') #;indDF
    indDF_repertoir <- updateMemory(indDF_repertoir = indDF_repertoir, indDF = indDF, mod=setup$mod) #;indDF_repertoir
      alive <- which(indDF[,"nRound"]>0)
      learn <- which( !is.na(indDF[,'learn']) & (indDF[,"nRound"] > 0) )
      indDF_collectedPayoff[ cbind(alive,indDF[alive,"nRound"]) ] <- indDF[alive,"yield"]
      indDF_collectedPayoff[ cbind(learn,indDF[learn,"nRound"]) ] <- NA #;indDF_collectedPayoff[,1:10] # use NAs to indicate learning turns; otherwise they overly increase the variance
	  indDF           <- updatePayoffs(indDF = indDF, indDF_collectedPayoff=indDF_collectedPayoff, setup = setup) #;indDF
    indDF           <- ageAndDie(setup = setup, indDF = indDF, mod='') ;indDF
    if(sum(indDF[,'nRound']<0) > 0){
      for(h in which(indDF[,'nRound']<0)){
        indDF_repertoir[h,1:(setup$nPat+1)] <- NA
        indDF_collectedPayoff[h,] <- NA
      }
        indDF <- sexreproduction(setup = setup, indDF = indDF) ;indDF
    }

    fems[time] <- mean(indDF[ indDF[,"S"]==1, "innovateProp"])
    mems[time] <- mean(indDF[ indDF[,"S"]==0, "innovateProp"])

    allSame <- length(unique(indDF[,"innovateProp"]))==1
    if(time == setup$nRound) allSame <- T

  }


  ### Report additional values from the simulation
  # patchesIL <- which(colSums(!is.na(indDF_repertoir[indDF[,"innovateProp"]==1, ]))>0) # average known patch values
  # patchesSL <- which(colSums(!is.na(indDF_repertoir[indDF[,"innovateProp"]==0, ]))>0) # average known patch values
  # payIL <- ifelse(length(patchesIL)>0, mean(patDF[patchesIL, "payoff"]), 0)
  # paySL <- ifelse(length(patchesSL)>0, mean(patDF[patchesSL, "payoff"]), 0)
  # diffpatIL <- ifelse(length(patchesIL)>0, length(patchesIL), 0)
  # diffpatSL <- ifelse(length(patchesSL)>0, length(patchesSL), 0)
  femIL <- mean(tail(fems, 1000), na.rm=T)
  memIL <- mean(tail(mems, 1000), na.rm=T)

  # return(cbind(indDF, setup, payIL, paySL, diffpatIL, diffpatSL))
  return(cbind(indDF, setup, femIL, memIL, time))
} # END RUN





#### Setting parameters for simulations on cluster >>>>>>>>>>>>>>>>>
variances <- 100#c(1,100) # payoff variance
mean <- 4 # the average payoff in the world
k_seq <- (mean^2)/variances # k parameter for the gamma distribution

# sequence for the environmental payoffChangeRate values
e_seq <- 10^-1.5#10^seq(-3,0,by=.5) # environmental turnover

# create all possible combinations of parameters
# grid <- expand.grid(k=k_seq,e=e_seq, x=c(0.01,0.02,0.04,0.08,seq(from=0.1,to=1,by=.1)), p=c(0.5), h=c(T,F), r=c(.20), c=c(1), rep=1:10)
grid <- expand.grid(k=k_seq,e=e_seq, xm=c(0.01,0.02,0.04,0.08,seq(from=0.1,to=1,by=.1)), xf=c(0.01,0.02,0.04,0.08,seq(from=0.1,to=1,by=.1)), m=0, p=c(.5), h=c(F), r=c(.20), c=c(1), rep=1:5)
#grid <- grid[grid[,"xf"]>grid[,"xm"],]

# for(queue in 456:910){

# choose individual parameters for the current queue
K <- grid[queue, 'k']
PCR <- grid[queue, 'e']
XM <- grid[queue, 'xm']
XF <- grid[queue, 'xf']
M <- grid[queue, 'm']
HFIE <- grid[queue, 'h']
PROP <- grid[queue, 'p']
R <- grid[queue, 'r']
COMP <- grid[queue, 'c']

### Define simulation parameters
setup <- data.frame(
  nRound = 10000, 			# number of rounds
  nInd = 100, 				# number of individuals
  nPat = 100, 				# number of patches
  payoffChangeRate = PCR, 		# payoff change rate
  payoffDis = "gamma", 			# resource distribution
  strategyProportion = PROP, 		# how many Individual Learner/Learning # use 2 for mixed/free learning
  competition = TRUE, 			# competition TRUE or FALSE
  deathRate = 1/100,			# death rate
  maxAge = 100, 			# maximum age
  propStable = FALSE,			# is social learning evolving? # freely evolving = false
  sexRatioStable = TRUE, 		# is the sex ration evolving? #freely evolving = false
  mutationRate = 0.01, 		# mutation rate
  repetition = 1, 			# number of repetitions
  k = K, 				# shape of gamma distribution
  theta = 4/K, 				# scale of gamma distribution
  lethalILearning = 0, 			# probability that learning ends with individual dying
  competitionStrength = COMP, 		# strength of resource competition
  mod = '', 				# model modifier
  philopatry = FALSE, 			# are individuals philopatric?
  centralIntel = FALSE, 		# is there central intelligence?
  #share = SHARE, 			#exclude when not in use !
  SexStratProp = .5, 			#1 == 100% female strategy, 0 == 100% male strategy
  xm = XM, 				#proportion of MALES   with highest fitness that can actually reproduce
  xf = XF, 				#proportion of FEMALES with highest fitness that can actually reproduce
  minIncome = M, 			# minimum income to reproduce
  highestFitnessIsEverything = HFIE, 	# are individulas in the top beta proportion equally likely to reproduce (FALSE) or relative to their fitness proxy (TRUE)
  temporalDiscRate = R, 		# temporal discount rate
  stringsAsFactors = FALSE # thats an argument to data.frame()
)


# Call simulation to run (repeatedly)
res <- bind_rows(lapply(1:10, function(i) run(setup=setup))); table(res[,"innovateProp"]); sum(res[,"innovateProp"]==1)/nrow(res)
# Move working directory to output
if(!file.exists('output')) dir.create(path = 'output'); setwd('output')
# Store results as individual files
save(res, setup, file=paste(paste(format(Sys.time(), '%y%m%d'),'learning',queue, sep="_"), sep='/'))
# }
