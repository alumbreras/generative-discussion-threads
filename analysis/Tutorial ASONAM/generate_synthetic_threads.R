# Create a set of synthetic thread with parameters alpha, beta, tau. 
# Find estimation of the parameters  alpha*, beta*, tau* 
# Compare structural properties of trees generated 
# with real and estimated parameters
devtools::load_all()

alpha <- 0.0
beta <- 1
tau <- 0.2
n <- 100

# Generate an plot synthetic tree ----------------------------------------------
parents <- gen.parentsvector.Gomez2013(n, alpha, beta, tau) # generate
plot.tree.nicely(parents)                                      # plot 
plot.tree.nicely.sequential(parents, stepsecs = 0)             # plot sequential


# Generate N synthetic trees (and plot some) -----------------------------------
ntrees <- 1000
parents <- replicate(ntrees,
                     gen.parentsvector.Gomez2013(n, alpha, beta, tau), 
                     simplify = FALSE)
par(mfrow = c(3,3))
sapply(1:9, function(idtree) plot.tree.nicely(parents[[idtree]])) # Plot some


# Estimate paremeters ----------------------------------------------------------

# Store in dataframe format. 
# Each line contains the post id, the chosen parent
# and the features of its parent (popularity, lag, root) at the 
# moment (t) of that choice.
df.trees <- all_parents_to_dataframe(parents)        

# Grid search of one parameter given the other two 
# This is useful to see the shape of the likelihood around the MLE.
par(mfrow = c(3,1))
alpha_grid <- seq(0.5,2, by = 0.01)
like <- rep(NA, length(alpha_grid))
for(i in 1:length(alpha_grid)){
  like[i] <- likelihood_Gomez2013(df.trees, alpha_grid[i], beta, tau)
}
plot(alpha_grid, like, xlab = 'alpha')
abline(v=alpha, col = 'blue')

beta_grid <- seq(0,10, by = 0.1)
like <- rep(NA, length(beta_grid))
for(i in 1:length(beta_grid)){
  like[i] <- likelihood_Gomez2013(df.trees, alpha=alpha, beta_grid[i], tau)
}
plot(beta_grid, like, xlab = 'beta')
abline(v=beta, col = 'blue')

tau_grid <- seq(0,1, by = 0.01)
like <- rep(NA, length(tau_grid))
for(i in 1:length(tau_grid)){
  like[i] <- likelihood_Gomez2013(df.trees, alpha, beta, tau_grid[i])
}
plot(tau_grid, like, xlab = 'tau')
abline(v=tau, col = 'blue')

# Estimate alpha, beta, tau parameters
res <- estimation_Gomez2013(df.trees = df.trees, params=list(alpha=0.5, beta=0.6, tau=0.5))
res

# Generate threads with the estimated parameters
parents_hat <- replicate(ntrees,
                         gen.parentsvector.Gomez2013(n, res$alpha, res$beta, res$tau), 
                         simplify = FALSE)

# Compare structural properties ------------------------------------------------

# Degree distribution
df.degrees <- degree_distribution(parents)
df.degrees_hat <- degree_distribution(parents_hat)

df.degrees$cumprob <- cumsum(df.degrees$frequency/sum(df.degrees$frequency))
df.degrees_hat$cumprob <- cumsum(
                      df.degrees_hat$frequency/sum(df.degrees_hat$frequency)
                      )            

df.degrees$data <- 'real'
df.degrees_hat$data <- 'estimated'
df.degrees <- bind_rows(df.degrees, df.degrees_hat)
ggplot(df.degrees, aes(x=degree, y = cumprob)) + 
  geom_point() +
  scale_y_log10() +
  facet_grid(.~data) +
  theme_bw() +
  ylab('CPF')

# Subtree size distribution
df.subtrees <- degree_distribution(parents)
df.subtrees_hat <- degree_distribution(parents_hat)

df.subtrees$data <- 'real'
df.subtrees_hat$data <- 'estimated'