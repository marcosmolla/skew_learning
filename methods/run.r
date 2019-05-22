### Defining run routine
run <- function(setup, ceed=NULL){
  # Create and store seed information
  if(is.null(ceed)) ceed <- round(runif(1,0,99999999))
  set.seed(ceed)
  setup$seed <- ceed
  
  ### Patches in the world
  patDF <- as.matrix(data.frame(id=0:(setup$nPat), payoff=0, agentsPresent=NA, competitionPayOff=NA))
  ### Single data to be stored for every living individual (row-wise)
  indDF <- as.matrix(data.frame(id=1:setup$nInd, nRound=1, nExploit=0, innovateProp=NA, atPatch=0, learn=NA, fitness=0, yield=0, yield2=0, S=0, fitness_variance=0, m1=NA,f1=NA)) 
  
  ### Multiple data stored for every individual (column-wise)
  indDF_repertoir <- matrix(NA,nrow=setup$nInd, ncol=(setup$nPat+1)) # nrow is max 100 because all individuals die
  indDF_collectedPayoff <- matrix(NA, ncol=setup$maxAge, nrow=setup$nInd) # columns are rounds, rows are indviduals
  
  patDF <- changeWorld(patDF=patDF, pat=nrow(patDF), create = T, payoffDis = as.character(setup$payoffDis), mod = '', setup=setup)
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
    patDF           <- changeWorld(patDF=patDF, pat=nrow(patDF), create = F, payoffDis = as.character(setup$payoffDis), mod='', setup=setup)
    observable 			<- observation(indDF)
    indDF 					<- whichAction(indDF = indDF, nInd = setup$nInd, indDF_repertoir = indDF_repertoir, patDF=patDF, setup=setup)
    indDF 					<- whichLearning(nInd = setup$nInd, indDF = indDF)
    indDF_repertoir <- formingMemory(setup = setup, patDF = patDF, indDF = indDF, indDF_repertoir = indDF_repertoir, observable = observable, mod='')
    patDF           <- individualsAtPatch(patDF = patDF, indDF = indDF, setup=setup)
    indDF           <- whichPayoff(setup = setup, patDF = patDF, indDF = indDF, mod = '')
    indDF_repertoir <- updateMemory(indDF_repertoir = indDF_repertoir, indDF = indDF, mod=setup$mod)
    alive <- which(indDF[,"nRound"]>0)
    learn <- which( !is.na(indDF[,'learn']) & (indDF[,"nRound"] > 0) )
    indDF_collectedPayoff[ cbind(alive,indDF[alive,"nRound"]) ] <- indDF[alive,"yield"]
    indDF_collectedPayoff[ cbind(learn,indDF[learn,"nRound"]) ] <- NA # use NAs to indicate learning turns; otherwise they overly increase the variance
    indDF           <- updatePayoffs(indDF = indDF, indDF_collectedPayoff=indDF_collectedPayoff, setup = setup)
    indDF           <- ageAndDie(setup = setup, indDF = indDF, mod='')
    
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
