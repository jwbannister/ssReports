# one-off functions written in a hurry to satisfy Brian Schmids request for 
# pm10 and wind roses. 

pm_plot <- function(locs, df1, background){
    a <- list(grobs=c(), centers=c())
    valueseq <- c(10, 50, 150, 500)
    legend.plot <- df1 %>% filter(deployment==df1$deployment[1]) %>%
        plot_rose(., value='pm10', dir='wd', valueseq=valueseq,
                  legend.title=bquote('P'*M[10]~'('*mu*'g/'*m^3*')'))
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
        annotation_custom(legnd, xmin=-116.1, xmax=-116.4,
                          ymin=32.65 , ymax=32.75) +
        coord_fixed() +
        ggtitle(paste0("Salton Sea - ", 
                       format(max(df1$datetime), '%Y-%m-%d %H:%M:%S')))
    for (i in 1:length(a$grobs)){
        p3 <- p3 + annotation_custom(a$grobs[[i]], xmin=a$centers[[i]][1]-.3,
                                     xmax=a$centers[[i]][1]+.3, 
                                     ymin=a$centers[[i]][2]-.3,
                                     ymax=a$centers[[i]][2]+.3) 
    }
    p3
}

wind_plot <- function(locs, df1, background){
    a <- list(grobs=c(), centers=c())
    valueseq <- c(2, 5, 9, 13)
    legend.plot <- df1 %>% filter(deployment==df1$deployment[1]) %>%
        plot_rose(., value='ws', dir='wd', valueseq=valueseq,
                  legend.title="Wind Speed (m/s)")
    legnd <- g_legend(legend.plot)
    fl <- tempfile()
    for (j in unique(df1$deployment)){
        p <- filter(df1, deployment==j) %>% 
            plot_rose_image_only(., value='ws', dir='wd', valueseq=valueseq)
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
        annotation_custom(legnd, xmin=-116.16, xmax=-116.46,
                          ymin=33 , ymax=33.1) +
        coord_fixed() +
        ggtitle(paste0("Salton Sea - ", 
                       format(max(df1$datetime), '%Y-%m-%d %H:%M:%S')))
    for (i in 1:length(a$grobs)){
        p3 <- p3 + annotation_custom(a$grobs[[i]], xmin=a$centers[[i]][1]-.3,
                                     xmax=a$centers[[i]][1]+.3, 
                                     ymin=a$centers[[i]][2]-.3,
                                     ymax=a$centers[[i]][2]+.3) 
    }
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
  dev.off()
  return(legend)
} 

#' Plot DCA areas with background
#' 
#' @import dplyr
#' @import ggplot2
#' @param polys_df 
#' @param labels_df 
#' @param external_points 
plot_extended_background <- function(){
    map <- raster::stack("~/gis/salton/landsat8_salton_120m.tif")
    ext <- sp::SpatialPointsDataFrame(coords=data.frame(x=c(555474, 650311), 
                                                        y=c(3751230, 3596987)), 
                                      data=data.frame(id=1:2), 
                                      proj4string=raster::crs(map))
    map_sub <- raster::crop(map, raster::extent(ext))
    map_sub <- raster::aggregate(map_sub, 4)
    map_df <- raster::as.data.frame(map_sub, xy=T)
    map_df <- data.frame(x=map_df$x, y=map_df$y, r=map_df[ , 3], g=map_df[ , 4], 
                         b=map_df[ , 5])
    map_df[is.na(map_df)] <- 0
    for (i in c('r', 'g', 'b')){
        map_df[[i]] <- 
        sapply(map_df[[i]], function(x) 
        (x - min(map_df[[i]])) * (255/(max(map_df[[i]]) - min(map_df[[i]]))))
    }
    p1 <- ggplot(data=map_df) + coord_fixed() + theme_bw() +
        geom_tile(aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255)), alpha=0.75) + 
        scale_fill_identity() + 
        scale_x_continuous(breaks=range(map_df$x)*c(1.01, 0.99), 
                           labels=range(map_df$x), expand = c(0,0)) +
        scale_y_continuous(breaks=range(map_df$y)*c(0.99, 1.01), 
                           labels=range(map_df$y), expand = c(0,0)) +
        theme(panel.grid=element_blank(), 
              axis.title=element_blank(), 
              plot.title=element_text(hjust=0.5))
    p1
}

plot_wind_background <- function(){
    map <- raster::stack("~/gis/salton/landsat8_salton_120m.tif")
  ext <- sp::SpatialPointsDataFrame(coords=data.frame(x=c(-116.47, -115.33), 
                                                 y=c(33.89, 32.9)), 
                                data=data.frame(id=1:2), 
                                proj4string=raster::crs(map))
  map_sub <- raster::crop(map, raster::extent(ext))
  map_sub <- raster::aggregate(map_sub, 4)
  map_df <- raster::as.data.frame(map_sub, xy=T)
  map_df <- data.frame(x=map_df$x, y=map_df$y, r=map_df[ , 3], g=map_df[ , 4], 
                       b=map_df[ , 5])
  map_df[is.na(map_df)] <- 0
  for (i in c('r', 'g', 'b')){
      map_df[[i]] <- 
          sapply(map_df[[i]], 
                 function(x) 
                     (x - min(map_df[[i]])) * (255/(max(map_df[[i]]) - min(map_df[[i]]))))
  }
  p1 <- ggplot(data=map_df) + coord_fixed() + theme_bw() +
  geom_tile(aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255)), alpha=0.75) + 
  scale_fill_identity() + 
  scale_x_continuous(breaks=range(map_df$x)*c(1.01, 0.99), 
                     labels=range(map_df$x), expand = c(0,0)) +
  scale_y_continuous(breaks=range(map_df$y)*c(0.99, 1.01), 
                     labels=range(map_df$y), expand = c(0,0)) +
  theme(panel.grid=element_blank(), 
        axis.title=element_blank(), 
        plot.title=element_text(hjust=0.5))
p1
}

brian_rose_image_only <- function(data,
                                 value,
                                 dir,
                                 dirres = 22.5,
                                 valuemin = 0,
                                 valueseq,
                                 palette = "YlOrRd", 
                                 reverse.bars=T){

  data <- data[data[[value]]>valuemin, ] 
  ifelse(missing(valueseq),
         valueseq <- round(seq(valuemin, max(data[[value]]), 
                               (max(data[[value]]) - valuemin) / 5), 0),
         valueseq <- c(valuemin, valueseq))
  # get some information about the number of bins, etc.
  n.value.seq <- length(valueseq)
  n.colors.in.range <- n.value.seq 
  # create the color map
  value.colors <- 
    colorRampPalette(RColorBrewer::brewer.pal(min(max(3,
                                                      n.colors.in.range),
                                                  min(9,
                                                      n.colors.in.range)),                                                 palette))(n.colors.in.range)
  value.breaks <- c(valueseq, max(data[[value]]) + 1)
  value.labels <- c(paste(c(valueseq[1:n.value.seq-1]), '-',
                          c(valueseq[2:n.value.seq])), 
                    paste0(valueseq[n.value.seq], "+"))
  data$value.binned <- cut(x = data[[value]],
                           breaks = value.breaks,
                           labels = value.labels,
                           ordered_result = TRUE)
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                  "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  # summarize data
  data_sum <- data %>% dplyr::group_by(dir.binned, value.binned) %>%
    summarize(value.prcnt = length(value.binned))
  prcnt_sums <- data_sum %>% group_by(dir.binned) %>%
    summarize(total.prcnt = sum(value.prcnt))
 # reverse order of bar factors if desired
  if (reverse.bars){
      value.colors <- rev(value.colors)
      data_sum$value.binned <- factor(data_sum$value.binned, 
                                      levels=rev(levels(data_sum$value.binned)),
                                      ordered=T)
  }
  # create the plot ----
  p.rose <- ggplot(data = data_sum,
                   aes(x = dir.binned, y = value.prcnt,
                       fill = value.binned)) +
geom_bar(stat='identity', color="black") + 
scale_x_discrete(drop = FALSE,
                 labels = waiver()) +
coord_polar(start = -((dirres/2)/360) * 2*pi) +
scale_fill_manual(values = value.colors,
                  drop = FALSE) +
ylim(0, 12) +
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
      plot.background=element_blank())
return(p.rose)
}
