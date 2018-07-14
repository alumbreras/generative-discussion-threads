alpha <- 100
beta <- 0
tau <- 1

# Synthetic trees --------------------------------------------------------------

# Generate a tree under Gomez model
parents = gen.parentsvector.Gomez2013(n=100, alpha, beta, tau)
plot.tree.nicely(parents)
plot.tree.nicely.sequential(parents, stepsecs = 0.1)

plot.tree.nicely.sequential(parents)

parents <- replicate(50,
                     gen.parentsvector.Gomez2013(n=50, alpha, beta, tau), simplify = FALSE)
plot.tree.nicely(parents[[1]])
df.trees <- all_parents_to_dataframe(parents)



parents = gen.parentsvector.Gomez2013(n=50, 1, 1, 0.1)
plot.tree.nicely(parents)