setGeneric('individualsAtPatch', function(patDF, indDF, setup) standardGeneric('individualsAtPatch'))
setMethod(f = 'individualsAtPatch', signature = c(patDF='matrix', indDF='matrix', setup='data.frame'), definition = function(patDF, indDF, setup){
	patDF[,'agentsPresent'] <- 0 # reset count
	patDF[,'competitionPayOff'] <- 0

	learn   <- indDF[,'learn']
	atPatch <- indDF[,'atPatch']
	if(any( is.na(learn) ) && any( atPatch>0 )){
		nAtPatchAndID <- table(atPatch[ atPatch != 0])
		nAtPatch <- as.numeric(nAtPatchAndID)

    	if(length(nAtPatch) != 0){
      	patchID <- as.numeric(names(nAtPatchAndID))
  			where <- patDF[,'id'] %in% patchID
    		patDF[,'agentsPresent'][ where ] <- nAtPatch
    	}

			  if(setup$competition){
			  	payoff <- patDF[,'payoff']
			  	patDF[,'competitionPayOff'] <- payoff
		  		patDF[,'competitionPayOff'][ where ] <- payoff[ where ] / (patDF[,'agentsPresent'][ where ] ^ setup$competitionStrength) # equivalent to: pi/(n^c) where c is the scaling factor of how severe number of individuals are
			  }
	}
  return(patDF)
})
