trees = gen.thread.Gomez2013(n=100, alpha=1, beta=1, tau=1)

g <- graph.empty(n=1)

# First post has no choice
g <- add_vertices(g, 1)
g <- add_edges(g, c(2,1))

for (i in 3:n){
  betas <- c(beta, rep(0, i-2))
  lags <- (i-1):1
  popularities <- 1 + degree(g, mode="in") # even root starts with degree 1

  # Probability of choosing every node (only one is chosen)
  probs <- alpha*popularities[1:(i-1)] + betas + tau^lags
  probs <- probs/sum(probs)
  j <- sample(1:length(probs), 1, prob=probs)

  # Add new vertex attached to the chosen node
  g <- add_vertices(g, 1)
  g <- add_edges(g, c(i,j))
}
g
