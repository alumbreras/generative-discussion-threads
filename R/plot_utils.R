library(gridExtra)
library(grid)

#' @description plots grid of ggplots
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  ggsave(file=paste0('ch4_structures_ ', subforum_, ', .png'),
         width=200, height=70, units='mm')
  combined
}


#' @title Plots a tree graph
#' @description Plots an igraph tree using nice parameters
#' @param gtree an igraph object with a tree structure (no cicles)
#' @param labels the type of label to be shown over vertices (if any)
#' @details This is a function to fast plot the structure of conversations.
#' If `label` is NA then no label is used.
#' @export
plot.tree <- function(gtree, labels=c('name', 'id')){
  if (missing(labels)){
    labels <- NA
  }
  else{
  labels <- switch(labels,
                   'name' = V(gtree)$name,
                   'id' = as.numeric(V(gtree)))
  }
  par(mfrow=c(1,1))
  V(gtree)$color <- gray.colors(vcount(gtree))
  V(gtree)[1]$color <- "red"
  gtree.un <- as.undirected(gtree)
  la = layout_as_tree(gtree.un, mode='out', root=which.min(V(gtree.un)$date))

  plot(gtree.un,
       layout = la,
       vertex.label = labels,
       vertex.size=3,
       edge.arrow.size=0.6)
}


plot.tree.nicely <- function(gtree){
  par(mfrow=c(1,1))
  V(gtree)$color <- gray.colors(vcount(gtree))
  V(gtree)[1]$color <- "red"
  gtree.un <- as.undirected(gtree)
  la = layout_nicely(gtree.un)
  plot(gtree.un,
       layout = la,
       vertex.label = NA,
       vertex.size=3)
}

