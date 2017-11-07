
# Generate a tree under Gomez model
parents = gen.parentsvector.Gomez2013(n=100, alpha=10, beta=0.1, tau=0.1)
parents <- replicate(10000,
                   gen.parentsvector.Gomez2013(n=20,
                                        alpha=10, beta=0.1, tau=0.1), simplify = FALSE)
plot.tree.nicely(parents_to_tree(parents[[100]]))
df.trees <- all_parents_to_dataframe(parents)

# Estimate its MLE parameters

# Grid search for one parameter andthe others fixed
x <- seq(0,40, by=0.1)
y = sapply(x, function(xx) likelihood_Gomez2013(df.trees, list(alpha=xx, beta=0.1, tau=0.1)))
plot(x, y)

# Estimation by optimization
res <- estimation_Gomez2013(df.tree)
cat("\n Estimated alpha:", res$alpha)
cat("\n Estimated beta:", res$beta)
cat("\n Estimated tau:", res$tau)
