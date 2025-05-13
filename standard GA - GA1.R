#STANDARD GA

library(readxl)
library(GA)
library(scales)


#monitor function - reports statistics after every generation. It overrides the default functionality provided by gaMonitor().
monitor <- function(obj){
  iter <- obj@iter    #current iteration number
  if (iter <= maxGenerations){    #error checking
    fitness <- obj@fitness        #array of all the fitness values in the present population
    thisRunResults[iter,1] <<- max(fitness) 
    thisRunResults[iter,2] <<- mean(fitness)
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }
  else{       #error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}

runGA <- function(maxGenerations, popSize, pcrossover, pmutation){
  noRuns = 30
  maxGenerations <<- maxGenerations
  popSize = popSize
  pcrossover = pcrossover
  pmutation = pmutation
  type = "permutation"
  data = getData()
  min = 1                 #minimum is city indexed 1
  max = nrow(getData())   #maximum is the number of cities in the data set
  fitness = tspFitness    #fitness function
  
  #Stats to report    
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) #stats of a single run
  resultsMatrix = matrix(1:maxGenerations, ncol = 1)  #stats of all the runs
  
  resultNames = character(length(statnames)*noRuns)
  resultNames[1] = "Generation"
  
  bestFitness <<- -Inf
  bestSolution <<- NULL

  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    GA <- ga(type = type, fitness = fitness, distMatrix = data, 
             min = min, max = max, popSize = popSize, maxiter = maxGenerations,
             pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i)
    
    #stats from each run recorded in thisRunResults.
    #After each run, results from thisRunResults dumped into larger matrix, resultsMatrix, using cbind().
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    #Results from ALL runs added to resultsMatrix. 
    #When For loop terminates, results of all runs available. Then results plotted to collectively show performance of all runs.
    
    #To know best solution produced across all runs, extract best result produced at end of present run using GA@fitnessValue.
    #Capital GA = object returned by function call to genetic algorithm (ga()).
    #GA object contains many variables. But GA@fitnessValue shows best fitness achieved in most recent run.
    #If this fitness better than previous, it is updated.
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
  }
  colnames(resultsMatrix) = resultNames
  return (resultsMatrix)
} 

getBestFitness<-function(){
  return(bestFitness)
}

getBestSolution<-function(){
  return(bestSolution)
}

getData <- function(){
  drone_urban_40 <- read_xlsx("Drone_Urban_40.xlsx", col_names = TRUE, range = "B2:AP42")
  distances <- as.matrix(drone_urban_40)
  return(distances)
}

tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])             #tour finishes where it started.
  route <- embed(tour, 2)[,2:1]        #converts the tour into a matrix of trips
  tourlength <- sum(distMatrix[route], na.rm = TRUE) 
  return(tourlength)                   
}

tspFitness <- function(tour, ...){
  return (1/tourLength(tour, ...))    #Since the tour length must be minimsed, 1/tourlength used to maximise.
}

plotTSPSolution<-function(solution, title){
  data <- dist(read_xlsx("Drone_Urban_40.xlsx", col_names = TRUE, range = "B2:AP42"))
  mds <- cmdscale(data) 
  x <- mds[, 1]
  y <- -mds[, 2]
  
  x_jitter <- jitter(x, amount = 0.1)
  y_jitter <- jitter(y, amount = 0.1)
  
  plot(x_jitter, y_jitter, type = "n", asp = 1, xlab = "X Coordinate", ylab = "Y Coordinate", main = title)
  
  abline(h = pretty(range(x), 10), v = pretty(range(y), 10), col = "light gray")
  
  #extract solution
  tour <- solution[1, ]
  tour <- c(tour, tour[1]) #add depot (assumed to be node 1) at start and end
  n <- length(tour)
  
  arrows(x_jitter[tour[-n]], y_jitter[tour[-n]], x_jitter[tour[-1]], y_jitter[tour[-1]],
         length = 0.15, angle = 25, col = alpha("blue", 0.5), lwd = 1)
  
  points(x_jitter, y_jitter, pch = 21, bg = "white", col = "black", cex = 1.5)
  
  text(x_jitter, y_jitter, labels = 1:length(x), cex = 0.5, col = "red")
}

parseData <- function(data, firstcolumn, noRuns){
  #3 arguments:
  #data - results matrix.
  #firstcolumn - index of first column where best fitness first appeared.
  #number of runs
  col <- firstcolumn
  
  allstats <- (ncol(data)-1)/noRuns   #how many stats were collected. Omit the first column (Generations)
  cols <- seq(col,noRuns*allstats, by=allstats)
  subdata <- data[,cols]
  noGens <- nrow(data)
  pdata <- matrix(nrow = noGens, ncol = 3)
  for (i in 1:noGens){
    pdata[i,1] = i
    pdata[i,2] = mean(subdata[i,])
    pdata[i,3] = 1.96*sd((subdata[i,]))/sqrt(noRuns)   #compute the length of error bar
  }
  return (pdata)
}

findminmax <- function(data, minimise = TRUE){
  minmax <- NA
  if (minimise) minmax <- min(data[,2])
  else minmax <- max(data[,2])
  
  rownum <- which(data[,2] == minmax)
  if (length(rownum) > 1) rownum <- rownum[1]
  
  if (minimise)
    return (minmax - data [rownum,3])
  else return (minmax + data [rownum,3])
}

#standard GA
ga1 <- runGA(maxGenerations = 100, popSize = 400, pcrossover = 0.6, pmutation = 0.1)
ga1 #to see results to calculate averages
ga1BestSolution <- getBestSolution() #obtain best solution
ga1BestSolution #to see what the solution looks like
plotTSPSolution(ga1BestSolution, "Optimised Drone Route - GA1")  #plot that best solution

#To calculate averages, print ga1 before parsing. Parsing requires following from the output:
#index of first column where best fitness first appeared = 2, 1st column is generations
#no of runs = 30

#Calling parseData for GA run
ga1_parsed <- parseData(ga1, 2, 30)
ga1_parsed
#Output of ga1_parsed:
#[,1] - column of generation numbers.     
#[,2] - mean value of best fitness at each generation.       
#[,3] - how much best fitness's were spread around mean value (AKA error bar).