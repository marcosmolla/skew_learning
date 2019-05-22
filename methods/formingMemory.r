setPayoffs <- function(setup, patDF){
	if(setup$competition && sum(patDF[,'competitionPayOff'],na.rm = T)!=0) {
		patPayoff <- patDF[,'competitionPayOff']
	} else {
		patPayoff <- patDF[,'payoff']
	}
	return(patPayoff)
}

choosePatches <- function(setup, patPayoff, observable, ID2, patDF){
	observed <- unlist(lapply(observable[ID2], function(what){
		if(!prefL){
			mySample(what[what!=0], size = 1, replace = T)
		} else {
			pay <- patPayoff[match(what,patDF[,'id'])]
			ifelse(test=length( what[what!=0] )!=0, yes=mySample(what[which(pay==max(pay,na.rm=T))],1), no=NA)
		}
	}))
	# in the next step this becomes 0, this is the case when there was nothing to observe and mySample returns an NA, than this individual learned about patch 0
	observed[is.na(observed)] <- 0
	return(observed)
}






setGeneric('formingMemory', function(setup, patDF, indDF, indDF_repertoir, indDF_repertoir_source, observable, mod, net) standardGeneric('formingMemory'))
setMethod(f = 'formingMemory', signature = c(setup='data.frame', patDF='matrix', indDF='matrix', indDF_repertoir='ANY', indDF_repertoir_source='ANY', observable='ANY', mod='character', net='ANY'), definition = function(setup, patDF, indDF, indDF_repertoir, indDF_repertoir_source, observable, mod, net){

	### Consolidate repertoir due to Individual Learning
	patPayoff <- setPayoffs(setup, patDF)
  learn <- indDF[,'learn']
  ID <- which(learn == 1 & !is.na(learn)) #'innovate'

  if(length(ID) > 0){
    patches <- c(1:setup$nPat)
		learnAboutPatch <- sample(x = patches, size = length(ID), replace = TRUE)
	  if(sum(learnAboutPatch,na.rm = T) > 0){
	  	k <- cbind(ID, (learnAboutPatch+1))
	  	indDF_repertoir[k] <- patPayoff[ (learnAboutPatch+1) ]
	  }
  }

  ### Consolidate repertoir due to Social Learning
  ID2 <- indDF[,'id'][ learn == 0 & !is.na(learn)] #'imitate'

  if(is.matrix(observable)) token<-sum(observable[,2], na.rm = T) > 0 else token<-observable[2] > 0 #either matrix or vector
  if(length(ID2) > 0 && token){ # if someone observes and something is observable
	  	observed <- unlist(lapply(ID2, function(who){
				if(!is.matrix(observable)){ #there is only one line of results and it is transformed to a vector
					ifelse(who==observable[1], return(0), return(observable[2]))
				} else { #there is more than one line, a matrix is evaluated
		  		x <- observable[matrix(observable,ncol=2)[,1]!=who,2] # with this piece I avoid picking information from the pool that the observer added in the last round (as a demonstrator)
		  		mySample(n = x, size = 1, replace = T)
				}
	  	}))
	  	observed[is.na(observed)] <- 0
  }

if(exists('observed')){ 
	  j <- cbind(ID2, (observed+1))
		indDF_repertoir[j] <- patPayoff[ (observed+1) ] # now individuals learn which payoff the patch has, i.e. if there are already individuals they learn competition payoff
	}


return(indDF_repertoir)

})
