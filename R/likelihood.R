# Functions to compute likelihoods of trees 
# under the different models (Gomez and Lumbreras)
# Author: Alberto Lumbreras

library(dplyr)
library(data.table)
library(igraph)
library(parallel)


#' @title Likelihood computation using the dataframe
#' @param row vector representing a post
#' @param alpha parameter alpha
#' @param beta parameter beta
#' @param tau parameter tau
#' @return loglikelihood of the post
#' @export
likelihood_post_Gomez2013 <- function(row, alpha, beta, tau){
  tau <- min(params$tau, 0.999999999) # avoid 1 so that we can keep using the 
  c(as.matrix(log(alpha * row['popularity'] + beta*row['root'] + tau^row['lag']) -
     log(2*alpha*(row['t']-1)   + beta + tau*(tau^row['t']-1)/(tau-1))))
}


#' @title Gomez likelihood 
#' @param df.tree data.frame with one post per row and features in columns.
#' @param alpha parameter alpha
#' @param beta parameter beta
#' @param tau parameter tau
#' @return loglikelihood of the dataset
#' @details df.tree must not have any non-numerical value since the internal apply
#' won't know how to deal with that
#' @export
# x100 times faster (for large dataframes)
likelihood_Gomez2013 <- function(df.trees, alpha, beta, tau){
  # avoid 1 so that we can keep using the # geometric series expansion
  tau <- min(tau, 0.999999999)
  
  sum(log(alpha*df.trees['popularity'] + beta*df.trees['root'] + tau^df.trees['lag']))-
  sum(log(2*alpha*(df.trees['t']-1)   + beta + tau*(tau^df.trees['t']-1)/(tau-1)))
}


#'@title Gomez likelihood
#'@param params list of parameters
#'@description like Gomez 2013 but does not make the sum
#' Needed during the EM for matrix computations
likelihood_Gomez2013_all <- function(df.trees, params){
  alpha <- params$alpha
  beta <- params$beta
  tau <- min(params$tau, 0.999999999) # avoid 1 so that we can keep using the 
                                      # geometric series expansion
  
  log(alpha*df.trees['popularity'] + beta*df.trees['root'] + tau^df.trees['lag'])-
    log(2*alpha*(df.trees['t']-1)   + beta + tau*(tau^df.trees['t']-1)/(tau-1))

}


#' The part of the lower bound that we optimize in the M-step
#' @param params vector of initial parameters
Qopt <- function(params, data, responsibilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsibilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  list.params <- list(alpha = params[1], beta = params[2], tau = params[3])
  data <- filter(data, t>1)
  a <- responsibilities[,k][data$id_]
  b <- apply(data, 1, function(x) likelihood_post(x, list.params))
  log(pis[k])*sum(responsibilities[,k]) + sum(a*b)
}

#'
#' @param params array of parameters
Qopt_opt <- function(params, data, responsibilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsibilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  list.params <- list(alpha = params[1], beta = params[2], tau = params[3])
  a <- responsibilities[,k][data$id_]
  b <- likelihood_Gomez2013_all(data, list.params)
  log(pis[k])*sum(responsibilities[,k]) + sum(a*b)
}


#' The part of the lower bound that we optimize in the M-step
Qopt.par <- function(params, data, responsibilities, pis, k){
  # E[lnp(X,Z|\theta)] likelihoods for clusters k and all users
  # given the current responsibilities
  # Note: pis do not affect the optimization. We include it so that the obtained value corresponds to
  # the complete Q equation
  # This is similar to Bishop eq. 9.40, except that we loop over users, not over posts
  list.params <- list(alpha = params[1], beta = params[2], tau = params[3])
  cl <- makeCluster(detectCores()-2)
  clusterExport(cl, c("likelihood_post", "params"))
  a <- responsibilities[,k][data$userint]
  b <- parApply(cl, data, 1, function(x) likelihood_post(x, list.params))
  stopCluster(cl)
  log(pis[k])*sum(responsibilities[,k]) + sum(a*b)
}

#' Total likelihood of a dataframe according to Lumbreras2016
#' @param data trees data.frame with one post per row and features in columns
#' @param model parameters
#' @param responsibilities responsibilities of users w.r.t clusters
#' @return loglikelihood of the dataset
#' @export
likelihood_Lumbreras2016 <- function(data, params, responsibilities, pis){

  if(! "user" %in% names(data)) stop("Missing column in data: user")

  alphas <- params$alpha
  betas <- params$beta
  taus <- params$tau
  like <- 0
  K <- length(alphas)

  # The internal id of a user is its row in the matrix of responsibilities
  user.realids <- unique(data$user)
  data$id_ <- match(data$user, rownames(responsibilities))
  data <- data %>% select(id_, t, popularity, parent, lag)

  # Check all users have a responsability entry
  if (!all(user.realids %in% rownames(responsibilities))){
    stop ('Some input users are not in the responsibility matrix')
  }


  # Q (see Bishop Eq. 9.40, p.443)

  # The next loop does the same than this one but in a vectorized way
  # Q <- 0
  # U <- length(unique(data$id_))
  #for(u in 1:U){
  #  Xu <- filter(data, userint==u) # all posts from user
  #  for(k in 1:K){
  #    Q <- Q + responsibilities[u,k]*(log(pis[k]) + sum(apply(Xu[-2], 1, likelihood_post, alphas[k], betas[k], taus[k])))
  #  }
  #}

  Q <- 0
  for(k in 1:K){
    Q <- Q + Qopt_opt(c(alphas[k], betas[k], taus[k]), data, responsibilities, pis, k)
    cat("Q, k, ", Q, k)
  }

  # Entropy of the posterior
  #entropies <- -responsibilities*log(responsibilities)
  #entropies[is.na(entropies)] <- 0 # covers cases of 0*log(0) (very rare)
  #entropy <- sum(entropies)
  entropy <- - sum(responsibilities * log(responsibilities), na.rm=TRUE)

  # Eq. 9.74, p.452
  like <- Q + entropy
  cat("\nQ: ", Q)
  cat("\nH: ", entropy)
  cat("\nTotal like: ", like, '\n')
  like
}


