# Calcualte Gini Index
gini <- function(x, na.rm = FALSE){
	if (!is.numeric(x)){
		warning("'x' is not numeric; returning NA")
		return(NA)
	}
	if (!na.rm && any(na.ind <- is.na(x)))
		stop("'x' contain NAs")
	if (na.rm)
		x <- x[!na.ind]
	n <- length(x)
	mu <- mean(x)
	N <- n * (n + 1)
	ox <- x[order(x)]
	dsum <- drop(2*crossprod(1:n,  ox))
	(dsum / (mu * N)) - 1
}
