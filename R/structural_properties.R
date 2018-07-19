#'@title Degree distribution
#'@param parents list of parent vectors
degree_distribution <-function(parents){
  # Create a dataframe with node-degree
  degrees <- c()
  for(pi in parents){
    degrees <- c(degrees, tabulate(pi, nbins=length(pi)))
  }
  
  # Compute the frequency of each degree. Include freq. of degree 0
  degrees.name <- 0:max(degrees) 
  frequencies <- tabulate(degrees+1)
  
  # Create a dataframe degree-frequency
  df.degrees <- data.frame(degree = degrees.name, frequency = frequencies)
  return(df.degrees)
}

#'@title Distribution of subtrees size
#'@description The subtree size for node i is the size of the tree 
#'of the descendants of i 
subtree_size_distribution <-function(parents){
  # Create a dataframe with node-degree
  degrees <- c()
  for(pi in parents){
    degrees <- c(degrees, tabulate(pi, nbins=length(pi)))
  }
}

size_depth <- function(parents){}

# PLOT FUNCTIONS

#'@title Plot a degree distribution
plot_degree_distribution <-function(df.degrees){
  ggplot(df.degrees, aes(x=degree, y = cumsum(frequency/sum(frequency)))) + 
    geom_point() +
    scale_y_log10() +
    theme_bw() +
    ylab('CPF')
}
