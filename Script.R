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
  sum <- sum + (returnRates1[i]-averageReturnRate1)*(returnRates2-averageReturnRate2)    
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

data
returnRatesData <- getReturnRatesData(data)
returnRatesData
averageReturnRatesData <- getAverageReturnRatesData(returnRatesData)
averageReturnRatesData
sampleVariancesData <- getSampleVariancesData(returnRatesData, averageReturnRatesData)
sampleVariancesData
standardDeviationsData <- getStandardDeviationsData(returnRatesData, averageReturnRatesData)
standardDeviationsData
