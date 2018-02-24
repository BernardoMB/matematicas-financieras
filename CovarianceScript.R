
getSampleCovariances <- function(averageReturnRatesRatio, returnRatesData) {
	N <- ncol(averageReturnRatesRatio) # Number of securities
	vector.of.pairs <- c() # which is a matrix 
	# Agarrar los pares.
	for(i in 1:N) {
		for (j in 1:N) {
			if (j > i) {
				# Se obtuvo el par (i,j)
				pair <- c(i,j)
				vector.of.pairs <- cbind(vector.of.pairs, pair)
			}
		}
	}
} 