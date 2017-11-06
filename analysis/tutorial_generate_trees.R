# Generate a tree under Gomez model
tree = gen.parentsvector.Gomez2013(n=100, alpha=10, beta=0, tau=0)
plot.tree.nicely(tree_from_parents_vector(tree))

# Estimate its MLE parameters
df.tree <- parentsvector.to.dataframe(tree)
res <- estimation_Gomez2013(df.tree)
cat("\n Estimated alpha:", res$alpha)
cat("\n Estimated beta:", res$beta)
cat("\n Estimated tau:", res$tau)
