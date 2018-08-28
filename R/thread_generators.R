# Generates artificial threads with models of Gomez 2010 and Gomez 2013
# author: Alberto Lumbreras

library(igraph)
library(dplyr)

#' @title Generate parents vector
#' @description parent vector is a tree encoded as a vector where a position i
#' encodes the parent of the node i
gen.parentsvector.Gomez2013 <- function(n=100, alpha=1, beta = 1, tau=0.75){

  if(n==1){
    return(1)
  }
  
  likelihood <- 0 
  df.trees <- data.frame()
  # n-1 parents because root has no parent
  pis <- rep(NA, n-1)
  
  # Second post (1st after root) has no choice, always choses the root
  pis[1] <- 1

  # Start from the 3rd post (2nd after root), which arrives at t=2
  for (t in 2:(n-1)){
    betas <- c(beta, rep(0, t-1))
    # Note that the latest post has lag = tau
    lags <- t:1
    # We consider an undirected graph, and every existing node has degree equal to one initially
    popularities    <- 1 + tabulate(pis, nbins=t)
    # but root has no outcoming link
    popularities[1] <- popularities[1] -1

    # Probability of choosing every node (only one is chosen)
    probs <- alpha * popularities + betas + tau^lags

    if(sum(probs) == 0) {
      probs = rep(1/t, t)
    } else {
      probs <- probs/sum(probs)
    }

    # Add new vertex attached to the chosen node
    pis[t] <- sample(length(probs), size=1, prob=probs)
  }
  return(pis)
}


#' Generates a tree given its posts authors
#' and the estimated parameters of the model (Lumbreras 2016)
#' @param params estimated model parameters
#' @param users sequence of users (equals to tree size)
#' @description Assumes that the identitity and cluster of users are known
gen.thread.Lumbreras2016 <- function(users, alphas, betas, taus){

  # add a fake root so that the index is easier to understand
  # i=1 -> root
  # i=2 -> first reply, etc
  #users <- c(NA, users) # removed since we include root in the users
  alphas <- c(NA, alphas)
  betas <- c(NA, betas)
  taus <- c(NA, taus)

  n <- length(users)
  if(n < 3) stop('Thread is too short')

  # First post has no choice
  g <- graph.empty(n=2)
  g <- add_edges(g, c(2,1))

  popularities <- rep(1,n)
  popularities[1] <- 2 # root has the initial one plus the first reply

  for (i in 3:n){

    # Get post parameters
    bs <- c(betas[i], rep(0, i-2))
    lags <- (i-1):1

    #popularities <- 1 + degree(g, mode="in") # even root starts with degree 1

    # Probability of choosing every node (only one is chosen)
    probs <- alphas[i]*popularities[1:(i-1)] + bs + taus[i]^lags
    probs <- probs/sum(probs)

    j <- sample(1:length(probs), 1, prob=probs)

    # update metrics t avoid using igraph (slower) for that
    popularities[j] <- popularities[j] + 1

    # Add new vertex attached to the chosen node
    g <- add_vertices(g, 1)
    g <- add_edges(g, c(i,j))
  }
  g
}


# Create dataframe from a set of trees
#####################################
if(FALSE){
  # Prepare all the information needed into a nice dataframe
  library(parallel)
  ncores <- detectCores() - 2
  cl<-makeCluster(ncores, outfile="", port=11439)
  clusterEvalQ(cl, {library(igraph); library(dplyr)})
  data.parlapply <- parLapply(cl, trees, tree.to.data)
  stopCluster(cl)

  # join results adding thread id
  for (i in 1:length(data.parlapply)){
    data.parlapply[[i]]$thread <- i
  }
  data.parlapply <- rbindlist(data.parlapply)
  df.trees <- data.frame(thread = data.parlapply$thread,
                         user = data.parlapply$user,
                         post = data.parlapply$post,
                         t = data.parlapply$t,
                         parent = data.parlapply$parent,
                         popularity = data.parlapply$popularity,
                         lag = data.parlapply$lag)

  save(df.trees, file='df.trees.Rda')

}
