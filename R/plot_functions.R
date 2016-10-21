
event_plot <- function(locs, df1, background){
  a <- list(grobs=c(), centers=c())
  valueseq <- c(10, 50, 150, 500)
  legend.plot <- df1 %>% filter(deployment==df1$deployment[1]) %>%
    plot_rose(., value='pm10', dir='wd_sonic', valueseq=valueseq,
              legend.title="PM10")
  legnd <- g_legend(legend.plot)
  for (j in unique(df1$deployment)){
    p <- filter(df1, deployment==j) %>% 
      plot_rose_image_only(., value='pm10', dir='wd_sonic', valueseq=valueseq)
    png(filename="./p.png", bg="transparent")
    print(p)
    dev.off()
    img <- png::readPNG("./p.png")
    ras <- grid::rasterGrob(img, interpolate=TRUE)
    a$grobs[[j]] <- ras
    a$centers[[j]] <- c(filter(locs, deployment==j)$x, 
                        filter(locs, deployment==j)$y)
  }
#  info <- ggplot_build(background)
#  xrange <- info[[2]]$ranges[[1]]$x.range
#  yrange <- info[[2]]$ranges[[1]]$y.range
#  buffer <- (xrange[2] - xrange[1])/10
#  coll.start <- format(as.Date(start.date), "%m/%d")
#  coll.end <- format(as.Date(end.date), "%m/%d/%Y")
#  plot.title <- paste0("TwB2 ", teom_locs$dca.group[1], 
#                       " PM10/Wind Roses (", coll.start, " - ", coll.end, ")")
  p3 <- ggplot() + geom_blank() + 
    annotation_custom(a$grobs[[1]], xmin=a$centers[[1]][1],
                      xmax=a$centers[[1]][1], 
                      ymin=a$centers[[1]][2],
                      ymax=a$centers[[1]][2]) +
#    annotation_custom(legnd, xmin=xrange[2], xmax=xrange[2],
#                      ymin = yrange[1], ymax=yrange[1]) +
    geom_label(data=locs, mapping=aes(x=x, y=y, label=deployment))+
#    ggtitle(plot.title) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          plot.title=element_text(size=12))
  file.remove("./p.png")
  p3
}

#' strip legend from ggplot object
#' 
#' @param a.gplot ggplot object.
#' @return A grob of the plot legend
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
} 
