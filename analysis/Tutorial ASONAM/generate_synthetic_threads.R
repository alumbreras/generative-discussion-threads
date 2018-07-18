# BUG: Para alpha 0, beta 1, tau 1 deberia todo el mundo ir a la raiz, no?
# (Repasar el modelo generativo en gen.parents...)
devtools::load_all()

alpha <- 0.25
beta <- 0.5
tau <- 0.75
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


# Store in dataframe format. Each line contains the post id, the chosen parent
# and the features of its parent (popularity, lag, root) at the 
# moment (t) of that choice.
df.trees <- all_parents_to_dataframe(parents)        

par(mfrow = c(3,1))
# Grid search of one parameter given the otther two
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
# TODO: we could do an EM and detect groups of trees with different parameters
# that would be a nice way to classify discussions in a forum.
# We could even go nonparametric (without conjugacy, but anyway... we could even use VB!)
# Could we even use NMF? F features, N trees. Features are the parent vector
# (we should put a F_max) But no. The generative model will not respect the tree 
# constrains.
res <- estimation_Gomez2013(df.trees = df.trees, params=list(alpha=0.5, beta=0.6, tau=0.5))
res


#####################################################



