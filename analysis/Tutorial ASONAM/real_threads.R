# Load parents vectors ---------------------------------------------------------
data("df.posts.france")

df.thread <- df.posts %>%
  group_by(thread) %>% arrange(date) %>% filter(n()>10) %>%
  mutate(pi = as.integer(match(parent, unique(parent))-1)) %>% 
  ungroup %>%
  arrange(thread, date)

parents <- df.thread %>% filter(pi > 0) %>% group_by(thread) %>%  
  do(thread=.$pi) %>%  ungroup()  %>%
  lapply(function(x) {(x)})
parents <- parents[[1]]

# Estimate parameters
df.trees <- all_parents_to_dataframe(parents)  
res <- estimation_Gomez2013(df.trees = df.trees, params=list(alpha=0.5, beta=0.6, tau=0.5))

# Generate threads with the estimated parameters
sizes <- sapply(parents, function(x) length(x))
parents_hat <- list()
for(i in 1:length(sizes)){
  parents_hat[[i]] <- gen.parentsvector.Gomez2013(sizes[i], res$alpha, res$beta, res$tau)
}


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
ggplot(df.degrees, aes(x=degree, y = frequency)) + 
  geom_point() +
  scale_y_log10() +
  facet_grid(.~data) +
  theme_bw() +
  ylab('CPF')