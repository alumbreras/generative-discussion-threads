#' @title Dataframe from parents vector
#' @description  build a dataframe from a parents vector. The dataset reflects
#' the choice made at every timestep and is a convenient format to compute the likelihood
parents_to_dataframe <- function(parents){
  # Hide the value of the first post after the root so that the + 1 extra term
  # in the degree (to account for output links) can also be used for the root,
  # which has no output link but has always at least the first input link
  if(length(parents)==1){
    df <- data.frame(post = 2,
                     t = 1,
                     parent = 1,
                     popularity = 0,
                     root = 1,
                     lag = 1)
    return(df)
  }
  
  parents_ <- parents
  parents_[1] <- -1
  popularities <- c(0,sapply(2:length(parents), 
                             function(t) 1 + sum(parents_[1:(t-1)]==parents[t])))
  
  
  posts <- 2:(length(parents)+1)
  df <- data.frame(post = posts,
                   t = 1:(length(parents)),
                   parent = parents) %>%
    mutate(popularity = popularities,
           root = ifelse(parent == 1, 1, 0),
           lag = t-parent+1)
  df
}

all_parents_to_dataframe <- function(parents_list){
  df.list <- list()
  for(i in 1:length(parents_list)){
    df.list[[i]] <- parents_to_dataframe(parents_list[[i]])
  }
  as.data.frame(rbindlist(df.list))
}

#' @title Tree from parent vectors
#  Build a real graph tree from a vector of parents
# 'params parent parents vector 2:N
parents_to_tree <- function(parents){
  size <- length(parents)+1
  g <- graph.empty(n=size)
  edges <- t(rbind(2:size, parents))
  graph_from_edgelist(edges)
}