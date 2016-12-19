
event_plot <- function(locs, df1, background){
    a <- list(grobs=c(), centers=c())
    valueseq <- c(10, 50, 150, 500)
    legend.plot <- df1 %>% filter(deployment==df1$deployment[1]) %>%
        plot_rose(., value='pm10', dir='wd', valueseq=valueseq,
                  legend.title="PM10")
    legnd <- g_legend(legend.plot)
    fl <- tempfile()
    for (j in unique(df1$deployment)){
        p <- filter(df1, deployment==j) %>% 
            plot_rose_image_only(., value='pm10', dir='wd', valueseq=valueseq)
        png(filename=fl, bg="transparent")
        print(p)
        dev.off()
        img <- png::readPNG(fl)
        ras <- grid::rasterGrob(img, interpolate=TRUE)
        a$grobs[[j]] <- ras
        a$centers[[j]] <- c(filter(locs, deployment==j)$lon_dd, 
                            filter(locs, deployment==j)$lat_dd)
    }
    p3 <- background + coord_cartesian() +  
        annotation_custom(legnd, xmin=-116, xmax=-116.3,
                          ymin=33.1 , ymax=33.2) +
        coord_fixed()
    for (i in 1:length(a$grobs)){
        p3 <- p3 + annotation_custom(a$grobs[[i]], xmin=a$centers[[i]][1]-.15,
                                     xmax=a$centers[[i]][1]+.15, 
                                     ymin=a$centers[[i]][2]-.15,
                                     ymax=a$centers[[i]][2]+.15) 
    }
    p3
    dev.off()
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
  dev.off()
} 
