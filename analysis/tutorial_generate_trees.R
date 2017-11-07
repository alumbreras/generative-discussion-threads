alpha <- 100
beta <- 0
tau <- 1

# Synthetic trees --------------------------------------------------------------

# Generate a tree under Gomez model
parents = gen.parentsvector.Gomez2013(n=10, alpha, beta, tau)
plot.tree.nicely(parents_to_tree(parents))

gtree = gen.thread.Gomez2013(n=100, alpha, beta, tau)
plot.tree.nicely(gtree)

parents <- replicate(5000,
           gen.parentsvector.Gomez2013(n=50, alpha, beta, tau), simplify = FALSE)
plot.tree.nicely(parents_to_tree(parents[[1]]))
df.trees <- all_parents_to_dataframe(parents)

# Estimate its MLE parameters

# Grid search for one parameter and the others fixed
x <- seq(80,10000, by=10)
y = sapply(x, function(xx) likelihood_Gomez2013(df.trees, list(alpha=xx, beta=beta, tau=tau)))
plot(x, y)

# Estimation by optimization
res <- estimation_Gomez2013(df.trees)
cat("\n Estimated alpha:", res$alpha)
cat("\n Estimated beta:", res$beta)
cat("\n Estimated tau:", res$tau)


# Real data --------------------------------------------------------------------
# Load a Reddit dataset already either in the form of a df.tree or in the
# form of a parents vector (maybe better, more universal)
