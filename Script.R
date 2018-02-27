time <- c(1,2,3,4,5,6,7)
prices.a <- c(57+6/8,59+7/8,59+3/8,55+4/8,56+2/8,59,60+2/8)
dividends.a <- c(0,0,0.725,0,0,0.725,0)
prices.b <- c(333,368,368+4/8,382+2/8,386,397+6/8,392)
dividends.b <- c(0,0,1.35,0,0,1.35,0)
prices.c <- c(106+6/8,108+2/8,124,122+2/8,135+4/8,141+6/8,165+6/8)
dividends.c <- c(0,0,0.4,0,0,0.42,0)

data <- data.frame(time, prices.a, dividends.a, prices.b, dividends.b, prices.c, dividends.c)
names(data) <- c('time', 'P.Security.1', 'D.Security.1', 'P.Security.2', 'D.Security.2', 'P.Security.3', 'D.Security.3')

getReturnRates <- function(prices, dividends) {
  returnRates <- c()
  n <- length(prices)
  for (i in 1:n-1) {
    returnRates <- c(returnRates, (prices[i+1]-prices[i]+dividends[i+1])/prices[i])    
  }
  returnRates
}

getAverageReturnRate <- function(returnRates) {
  sum <- 0
  n <- length(returnRates)
  for (i in 1:n) {
    sum <- sum + returnRates[i]
  }
  sum/n
}

getSampleVariance <- function(returnRates, averageReturnRate) {
  sum <- 0
  n <- length(returnRates)
  for (i in 1:n) {
    sum <- sum + (returnRates[i] - averageReturnRate)^2
  }
  sum/n
}

getSampleStandardDeviation <- function(sampleVariance) {
  sqrt(sampleVariance)
}

getSampleCovariance <- function(returnRates1, averageReturnRate1, returnRates2, averageReturnRate2) {
  sum <- 0
  n <- length(returnRates1)
  for (i in 1:n){
  sum <- sum + (returnRates1[i]-averageReturnRate1)*(returnRates2[i]-averageReturnRate2)    
  }
  sum/n
}

getSampleCorrelationCoeficient <- function(covariance, standardDeviation1, standardDeviation2) {
  covariance/standardDeviation1*standardDeviation2
}

getReturnRatesData <- function(data) {
  names <- c("Period")
  rates <- data.frame(1:(length(data[[1]])-1))
  N <- (ncol(data)-1)/2
  for (i in 1:N) {
    prices <- data[[i*2]]
    dividends <- data[[i*2+1]]
    returnRates <- getReturnRates(prices, dividends)
    names <- c(names, paste(c("Security.",i), collapse=""))
    rates <- cbind(rates, returnRates)
  }
  names(rates) <- names
  rates
}

getAverageReturnRatesData <- function(returnRatesdata) {
  names <- c()
  averageRates <- c()
  N <- ncol(returnRatesdata)-1
  for (i in 1:N) {
    returnRates <- returnRatesdata[[i+1]]
    averageReturnRate <- getAverageReturnRate(returnRates)
    averageRates <- cbind(averageRates, averageReturnRate)
    name <- paste(c("A.R.R.Security.",i), collapse="")
    names <- c(names, name)
  }
  averageRates <- data.frame(averageRates)
  names(averageRates) <- names
  averageRates
}

getSampleVariancesData <- function(returnRatesData, averageReturnRatesData) {
  names <- c()
  sampleVariances <- c()
  N <- ncol(returnRatesData)-1
  for (i in 1:N) {
    sum <- 0
    returnRates <- returnRatesData[[i+1]] 
    averageReturnRate <- averageReturnRatesData[[i]]
    sampleVariance <- getSampleVariance(returnRates, averageReturnRate)
    sampleVariances <- cbind(sampleVariances, sampleVariance)
    name <- paste(c("S.V.Security.",i), collapse="")
    names <- c(names, name)
  }
  sampleVariances <- data.frame(sampleVariances)
  names(sampleVariances) <- names
  sampleVariances
}

getStandardDeviationsData <- function(returnRatesData, averageReturnRatesData) {
  names <- c()
  standardDeviations <- c()
  N <- ncol(returnRatesData)-1
  for (i in 1:N) {
    sum <- 0
    returnRates <- returnRatesData[[i+1]] 
    averageReturnRate <- averageReturnRatesData[[i]]
    sampleVariance <- getSampleVariance(returnRates, averageReturnRate)
    standardDeviation <- getSampleStandardDeviation(sampleVariance)
    standardDeviations <- cbind(standardDeviations, standardDeviation)
    name <- paste(c("S.D.Security.",i), collapse="")
    names <- c(names, name)
  }
  standardDeviations <- data.frame(standardDeviations)
  names(standardDeviations) <- names
  standardDeviations
}

getSampleCovariancesData <- function(averageReturnRatesData, returnRatesData) {
  N <- ncol(averageReturnRatesData) # Number of securities
  vector.of.pairs <- c() # which is a matrix
  names <- c() # The names of the final data frame.
  sampleCovariances <-c()
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
        averageReturnRate2 <- averageReturnRatesData[[j]]
        sampleCovariance <- getSampleCovariance(returnRates1, averageReturnRate1, returnRates2, averageReturnRate2)
        sampleCovariances <- cbind(sampleCovariances, sampleCovariance)
        name <- paste(c("S.C.Security.",i,"-",j), collapse="")
        names <- c(names, name)
      }
    }
  }
  sampleCovariances <- data.frame(sampleCovariances)
  names(sampleCovariances) <- names
  sampleCovariances
}

getSampleCorrelationCoeficientsData <- function(sampleCovariancesData, standardDeviationsData) {
  N <- ncol(standardDeviationsData)
  k <- 1
  sampleCorrelationCoefficients <- c()
  names <- c()
  for (i in 1:N) {
    for (j in 1:N) {
      if (j > i) {
        sampleCovariance <- sampleCovariancesData[[k]]
        standardDeviation1 <- standardDeviationsData[[i]]
        standardDeviation2 <- standardDeviationsData[[j]]
        correlationCoefficient <- sampleCovariance/(standardDeviation1*standardDeviation2)
        sampleCorrelationCoefficients <- cbind(sampleCorrelationCoefficients, correlationCoefficient)
        name <- paste(c("S.C.C.Security.",i,"-",j), collapse="")
        names <- c(names, name)
        k <- k + 1
      }
    }
  }
  sampleCorrelationCoefficients <- data.frame(sampleCorrelationCoefficients)
  names(sampleCorrelationCoefficients) <- names
  sampleCorrelationCoefficients
}

data
returnRatesData <- getReturnRatesData(data)
returnRatesData
averageReturnRatesData <- getAverageReturnRatesData(returnRatesData)
averageReturnRatesData
sampleVariancesData <- getSampleVariancesData(returnRatesData, averageReturnRatesData)
sampleVariancesData
standardDeviationsData <- getStandardDeviationsData(returnRatesData, averageReturnRatesData)
standardDeviationsData
sampleCovariancesData <- getSampleCovariancesData(averageReturnRatesData, returnRatesData)
sampleCovariancesData
sampleCorrelationCoefficientsData <- getSampleCorrelationCoeficientsData(sampleCovariancesData, standardDeviationsData)
sampleCorrelationCoefficientsData

a <- averageReturnRatesData[[1]]-averageReturnRatesData[[2]]
b <- averageReturnRatesData[[2]]

c <- sampleVariancesData[[1]] + sampleVariancesData[[2]] + -2*standardDeviationsData[[1]]*standardDeviationsData[[2]]*sampleCorrelationCoefficientsData[[1]]
d <- -2*sampleVariancesData[[2]] + 2*standardDeviationsData[[1]]*standardDeviationsData[[2]]*sampleCorrelationCoefficientsData[[1]]
e <- sampleVariancesData[[2]]

f <- -b/a
g <- 1/a

h <- c*g^2
i <- c*2*f*g + d*g
j <- c*f^2 + d*f + e

library('ggplot2')

# Plot the Markowitz parable for assets 1 and 2
Sp.squared <- function(Rp) {
  h*Rp^2 + i*Rp + j
}
ggplot(data.frame(x=c(-1, 1)), aes(x=x)) + stat_function(fun=Sp.squared, geom="line", color='green') + xlab("x") + ylab("y")

# Plot the Markowitz curve for assets 1 and 2
dat<- data.frame(t=seq(0, 1, by=0.01))
Rp <- function(t) t*averageReturnRatesData[[1]] + (1-t)*averageReturnRatesData[[2]]
Sp2 <- function(t) (t^2)*sampleVariancesData[[1]] + ((1-t)^2)*sampleVariancesData[[2]] + 2*t*(1-t)*sampleCovariancesData[[1]]
Sp <- function(t) sqrt((t^2)*sampleVariancesData[[1]] + ((1-t)^2)*sampleVariancesData[[2]] + 2*t*(1-t)*standardDeviationsData[[1]]*standardDeviationsData[[2]]*sampleCorrelationCoefficientsData[[1]])
dat$y=Rp(dat$t)
dat$x=Sp(dat$t)
with(dat, plot(x,y, type="l", col="green"))
