#MEMETIC HYBRID GA - 2-opt local seach GA

tsp_fitness <- function(route) {
  total_dist <- 0
  n <- length(route)
  for (i in 1:(n - 1)) {
    total_dist <- total_dist + dist_matrix[route[i], route[i + 1]]
  }
  total_dist <- total_dist + dist_matrix[route[n], route[1]]  #return to start
  return(-total_dist)  #GA maximizes, so negate for minimization
}

#uses k pairs
stochastic_two_opt <- function(route, dist_matrix, k = 390) {
n <- length(route)
best_route <- route
best_dist <- sum(dist_matrix[cbind(route[-n], route[-1])]) + dist_matrix[route[n], route[1]]

for (iter in 1:k) {
  i <- sample(1:(n - 1), 1)
  j <- sample((i + 1):n, 1)
  new_route <- replace(route, i:j, route[j:i])
  new_dist <- sum(dist_matrix[cbind(new_route[-n], new_route[-1])]) + dist_matrix[new_route[n], new_route[1]]
  if (new_dist < best_dist) {
    best_route <- new_route
    best_dist <- new_dist
  }
}
return(best_route)
}

#2-opt local search
two_opt <- function(route, dist_matrix) {
  n <- length(route)
  best_route <- route
  best_dist <- -tsp_fitness(route)
  improved <- TRUE
  
  while (improved) {
    improved <- FALSE
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        new_route <- route
        new_route[i:j] <- route[j:i]  #reverse segment
        new_dist <- -tsp_fitness(new_route)
        if (new_dist < best_dist) {
          best_route <- new_route
          best_dist <- new_dist
          improved <- TRUE
        }
      }
    }
    route <- best_route
  }
  return(best_route)
}

#Custom mutation operator with 2-opt
custom_mutation <- function(object, parent) {
  child <- as.vector(object@population[parent, ])
  child <- two_opt(child, dist_matrix)
  return(child)
}

#2-opt local search GA
ga2 <- runGA(maxGenerations = 100, popSize = 400, pcrossover = 0.6, pmutation = 0.1)
ga2 #to see results to calculate averages
ga2BestSolution <- getBestSolution() #obtain best solution
ga2BestSolution #to see what the solution looks like
plotTSPSolution(ga2BestSolution, "Optimised Drone Route - GA2 (2-opt Local Search)")  #plot best solution

#Calling parseData for GA run
ga2_parsed <- parseData(ga2, 2, 30) 
ga2_parsed
#Output of ga2_parsed:
#[,1] - column of generation numbers.     
#[,2] - mean value of best fitness at each generation.       
#[,3] - how much best fitness's were spread around mean value (AKA error bar).