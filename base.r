### Marco Smolla, Manchester University
### 22–05-2019

### Load libraries
  library(dplyr)

### Load methods
  tmpWD <- getwd() # retrieve working directory
  setwd(paste(tmpWD,"methods/",sep="/")) # change to methods directory
  l <- lapply(list.files(), source) # source methods
  setwd(tmpWD) # go back to working directory

### Set queue (I use this variable when running simulations on computer clusters with independent execution of jobs)
  queue <- 1

### Setting parameters for simulations
  variances <- 100            # payoff variance (1 = low, 100 = high)
  mean <- 4                   # mean payoff in the world
  k_seq <- (mean^2)/variances # calcualte k parameter of the gamma distribution used in the simualtion to draw random patch payoff values based on the chosen mean and variance
  e_seq <- 10^-1.5            # environmental turnover rate (probability that a patch changes its payoff in a given round)

# create all possible combinations of parameters
  grid <- expand.grid(k=k_seq, # k parameter of gamma distribution
                      e=e_seq, # environmental turnover
                      xm=c(0.01,0.02,0.04,0.08,seq(from=0.1,to=1,by=.1)), # proportion of reproductive males
                      xf=c(0.01,0.02,0.04,0.08,seq(from=0.1,to=1,by=.1)), # proportion of reproductive females
                      m=1, # amount of minimum income (1=1, 0=none)
                      p=c(.5,2), # proportion of social learning (0,...,1=none, ..., all social learner; 2=mixed learning)
                      h=c(T,F), # whether reproduction is relative or absolute to collected payoffs
                      r=c(.20), # temporal discount rate
                      c=c(1), # competition coefficient (0,...,1=no competition, ..., full scramble competition)
                      rep=1:5) # repetition of each parameter set
  
  #grid <- grid[grid[,"xf"]>grid[,"xm"],] # use this option to only simulate combinations where "males" have stronger reproductive skew than females (i.e. smaller proportion of males reproducing than in females)

# Loop over all parameter combinations
  # for(queue in 456:910){ 
    # select individual parameters for the current queue number
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
        nRound = 10000, 			      # number of rounds
        nInd = 100, 				        # number of individuals
        nPat = 100, 				        # number of patches
        payoffChangeRate = PCR, 		# payoff change rate
        payoffDis = "gamma", 			  # resource distribution
        strategyProportion = PROP, 	# how many Individual Learner/Learning # use 2 for mixed/free learning
        competition = TRUE, 			  # competition TRUE or FALSE
        deathRate = 1/100,			    # death rate
        maxAge = 100, 			        # maximum age
        propStable = FALSE,			    # is social learning evolving? # freely evolving = false
        sexRatioStable = TRUE, 		  # is the sex ration evolving? #freely evolving = false
        mutationRate = 0.01, 		    # mutation rate
        repetition = 1, 			      # number of repetitions
        k = K, 				              # shape of gamma distribution
        theta = 4/K, 				        # scale of gamma distribution
        lethalILearning = 0, 			  # probability that learning ends with individual dying
        competitionStrength = COMP, # strength of resource competition
        mod = '', 				          # model modifier
        philopatry = FALSE, 			  # are individuals philopatric?
        centralIntel = FALSE, 		  # is there central intelligence?
        SexStratProp = .5, 			    # 1 == 100% female strategy, 0 == 100% male strategy
        xm = XM, 				            # proportion of MALES   with highest fitness that can actually reproduce
        xf = XF, 				            # proportion of FEMALES with highest fitness that can actually reproduce
        minIncome = M, 			        # minimum income to reproduce
        highestFitnessIsEverything = HFIE, 	# are individulas in the top beta proportion equally likely to reproduce (FALSE) or relative to their fitness proxy (TRUE)
        temporalDiscRate = R, 		  # temporal discount rate
        stringsAsFactors = FALSE    # thats an argument to data.frame()
      )
    
    
    # Call simulation to run (repeatedly)
      res <- bind_rows(lapply(1:10, # repeats the same simulation 10 times in the same job call
                              function(i) run(setup=setup)))
    
    # Move working directory to output
      if(!file.exists('output')) dir.create(path = 'output'); setwd('output')
    
    # Store results as individual files
      save(res, setup, file=paste(paste(format(Sys.time(), '%y%m%d'),'learning',queue, sep="_"), sep='/'))
  # }
