
getSampleCovariances <- function(averageReturnRatesData, returnRatesData) {
	N <- ncol(averageReturnRatesData) # Number of securities
	vector.of.pairs <- c() # which is a matrix 
	# Agarrar los pares.
	for(i in 1:N) {
		for (j in 1:N) {
			if (j > i) {
				# Se obtuvo el par (i,j)
				pair <- c(i,j)
				vector.of.pairs <- cbind(vector.of.pairs, pair)
				returnRates1 <- returnRatesData[[1+i]]
				returnRates2 <- returnRatesData[[1+j]]
				averageReturnRate1 <- averageReturnRatesData[[i]]
				averageReturnRate1 <- averageReturnRatesData[[j]]
			}
		}
	}

} 