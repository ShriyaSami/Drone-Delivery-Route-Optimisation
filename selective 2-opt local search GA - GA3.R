#MEMETIC HYBRID GA - selective 2-opt local seach GA

#uses k pairs
stochastic_two_opt <- function(route, dist_matrix, k = 195) { #25% of total swaps
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

#Custom mutation operator with stochastic 2-opt
custom_mutation <- function(object, parent) {
  child <- as.vector(object@population[parent, ])
  child <- stochastic_two_opt(child, dist_matrix)
  return(child)
}

#selective 2-opt local seach GA
ga3 <- runGA(maxGenerations = 100, popSize = 400, pcrossover = 0.6, pmutation = 0.1)
ga3 #to see results to calculate averages
ga3BestSolution <- getBestSolution() #obtain best solution
ga3BestSolution #to see what the solution looks like
plotTSPSolution(ga3BestSolution, "Optimised Drone Route - GA3 (Selective 2-opt Local Search)")  #plot best solution

#Calling parseData for GA run
ga3_parsed <- parseData(ga3, 2, 30)
ga3_parsed
#Output of ga3_parsed:
#[,1] - column of generation numbers.     
#[,2] - mean value of best fitness at each generation.       
#[,3] - how much best fitness's were spread around mean value (AKA error bar).

#Line bar to compare fitness trends
plotbars<- function(data1, data2, data3, 
                    cap1 = "GA1", cap2 = "GA2", cap3 = "GA3"){
  data = data1
  hues = c("black","blue","green")
  
  min1 = findminmax(data1)   #min(data1) - data1 [which(data1 == min(data1))+2*nrow(data1)]
  min2 = findminmax(data2)   #min(data2) - data2 [which(data2 == min(data2))+nrow(data2)]
  min3 = findminmax(data3)   #min(data3) - data3 [which(data3 == min(data3))+nrow(data3)]
  
  max1 = findminmax(data1, FALSE)   #max(data1) + data1 [which(data1 == max(data1))+nrow(data1)]
  max2 = findminmax(data2, FALSE)   #max(data2) + data2 [which(data2 == max(data2))+nrow(data2)]
  max3 = findminmax(data3, FALSE)   #max(data3) + data3 [which(data3 == max(data3))+nrow(data3)]
  
  minn = min(min1, min2, min3)
  maxx = max(max1, max2, max3)
  
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar
  plot(df$x, df$y, type = "l", col = hues[1],  ylim=c(minn, maxx), #ylim = c(0.96, 0.985),  
       main = "Mean Fitness with Error Bars Across Generations", xlab = "Generations", ylab = "Fitness")  #plot the line (mean values)
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[1]);   
  
  data = data2
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
  lines(df$x, df$y, col = hues[2])
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[2]); 
  
  data = data3
  df <- data.frame(x=data[,1], y=data[,2], dy = data[,3])  #dy = length of error bar  
  lines(df$x, df$y, col = hues[3])
  segments(df$x, df$y - df$dy, df$x, df$y + df$dy, col = hues[3]); 
  
  legend("bottomright", legend = c(cap1, cap2, cap3), col = hues, lwd = 1, cex = 0.5)
}

plotbars(ga1_parsed, ga2_parsed, ga3_parsed, "GA1", "GA2 (2-opt Local Search)", "GA3 (Selective 2-opt Local Search)")


#Statistical Significance Tests

#number of runs (30 runs, each using 3 columns)
num_runs <- 30

#extracting best fitness values from ga1 (Standard GA)
ga1_best <- sapply(1:num_runs, function(i) {
  # The 'Best' column for run i is at (i-1)*3 + 2
  col_idx <- (i - 1) * 3 + 2
  tail(ga1[, col_idx], 1)
})

#extracting best fitness values from ga2 (2-opt GA)
ga2_best <- sapply(1:num_runs, function(i) {
  col_idx <- (i - 1) * 3 + 2
  tail(ga2[, col_idx], 1)
})

#extracting best fitness values from ga3 (Selective 2-opt GA)
ga3_best <- sapply(1:num_runs, function(i) {
  col_idx <- (i - 1) * 3 + 2
  tail(ga3[, col_idx], 1)
})

#collecting best fitness values from 3 GA types into a single data frame
best_fitnesses_df <- data.frame(
  Run = rep(1:num_runs, times = 3),
  GA_Type = rep(c("Standard_GA", "Opt2_GA", "Selective_Opt2_GA"), each = num_runs),
  Fitness_Value = c(ga1_best, ga2_best, ga3_best)
)

#Shapiro-wilk test for normality
shapiro.test(ga1_best)
shapiro.test(ga2_best)
shapiro.test(ga3_best)

#ANOVA stats test
anova_result <- aov(Fitness_Value ~ GA_Type, data = best_fitnesses_df)
summary(anova_result)

#TukeyHSD for pairwise comparisons
TukeyHSD(anova_result)
