# Create a set of synthetic thread with parameters alpha, beta, tau. 
# Find estimation of the parameters  alpha*, beta*, tau* 
# Compare structural properties of trees generated 
# with real and estimated parameters
devtools::load_all()
library(tidyr)

alpha <- 1
beta <- 1
tau <- 0.5
ntrees <- 100

# Generate an plot synthetic tree ----------------------------------------------
parents <- gen.parentsvector.Gomez2013(ntrees, alpha, beta, tau) # generate
plot.tree.nicely(parents)                                        # plot 
plot.tree.nicely.sequential(parents, stepsecs = 0)               # plot sequential


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

# Estimate parameters many times with different initialization
# to check robustness and possible bugs
# Estimate alpha, beta, tau parameters
alpha <- 2
beta <- 1
tau <- 0.5

df.trees <- all_parents_to_dataframe(parents)        
df.results <- data.frame()
for(ntrees in c(10, 1000)){
  
  # Generate trees
  parents <- replicate(ntrees,
                       gen.parentsvector.Gomez2013(n=25, alpha, beta, tau), 
                       simplify = FALSE)
  df.trees <- all_parents_to_dataframe(parents)        
    
  # Estimate with different init parameters
  for(xp in 1:10){
    alpha_0 <- runif(1)*10
    beta_0  <- runif(1)*10
    tau_0   <- runif(1, max=0.99)
    res <- estimation_Gomez2013(df.trees = df.trees, params=list(alpha = alpha_0, beta = beta_0, tau = tau_0))
    #res <- estimation_Gomez2013(df.trees = df.trees, params=list(alpha = 1, beta=1, tau = 0.2))
    res$ntrees <- ntrees
    df.results <- rbind(df.results, res)
  }
}

df.errors <- df.results 
df.errors$alpha <- df.errors$alpha # - alpha
df.errors$beta <- df.errors$beta   #- beta
df.errors$tau <- df.errors$tau     #- tau
df.errors <- gather(df.errors, param, value, -likelihood, -ntrees)
df.errors$param <- factor(df.errors$param, levels = c("beta", "alpha", "tau"))
ggplot(df.errors, aes(x=param, y= value)) + 
  #geom_point() + 
  geom_boxplot() + ylim(-5,5) +
  facet_grid(.~ntrees) + theme_bw()


# Simulating a real scenario
alpha <- 1
beta <- 1
tau <- 0.5
ntrees <- 100
parents <- replicate(ntrees,
                     gen.parentsvector.Gomez2013(n=100, alpha, beta, tau), 
                     simplify = FALSE)

res <- estimation_Gomez2013(df.trees = df.trees, params=list(alpha=0.5, beta=0.6, tau=0.5))

# Generate threads with the estimated parameters
parents_hat <- replicate(ntrees,
                         gen.parentsvector.Gomez2013(n=100, res$alpha, res$beta, res$tau), 
                         simplify = FALSE)

# Compare structural properties ------------------------------------------------

# Degree distribution
df.degrees     <- struct_degree_distribution(parents)
df.degrees_hat <- struct_degree_distribution(parents_hat)
df.degrees$cumprob     <- cumsum(df.degrees$frequency/sum(df.degrees$frequency))
df.degrees_hat$cumprob <- cumsum(
                           df.degrees_hat$frequency/sum(df.degrees_hat$frequency)
                          )            

df.degrees$data     <- 'real'
df.degrees_hat$data <- 'estimated'
df.degrees <- bind_rows(df.degrees, df.degrees_hat)
ggplot(df.degrees, aes(x=degree, y = cumprob)) + 
  geom_point() +
  scale_y_log10() +
  facet_grid(.~data) +
  theme_bw() +
  ylab('CPF')

# Subtree size distribution
df.subtrees     <- struct_subtree_size_distribution(parents)
df.subtrees_hat <- struct_subtree_size_distribution(parents_hat)
df.subtrees$cumprob     <- cumsum(df.subtrees$frequency/sum(df.subtrees$frequency))
df.subtrees_hat$cumprob <- cumsum(
                          df.subtrees_hat$frequency/sum(df.subtrees_hat$frequency)
                        )        

df.subtrees$data     <- 'real'
df.subtrees_hat$data <- 'estimated'
df.subtrees <- bind_rows(df.subtrees, df.subtrees_hat)
ggplot(df.subtrees, aes(x=size, y = cumprob)) + 
  geom_point() +
  scale_y_log10() +
  facet_grid(.~data) +
  theme_bw() +
  ylab('CPF')


# Size vs Depth
df.sizedepth     <- struct_size_depth(parents)
df.sizedepth_hat <- struct_size_depth(parents_hat)

df.sizedepth$data     <- 'real'
df.sizedepth_hat$data <- 'estimated'
df.sizedepth <- bind_rows(df.sizedepth, df.sizedepth_hat)
ggplot(df.sizedepth, aes(x=size, y = depth)) + 
  geom_point() +
  #scale_y_log10() +
  facet_grid(.~data) +
  theme_bw() +
  xlab("size") + ylab('depth')
