########################
## Libraries included ##
########################
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools) # and their dependencies
library(rgeos)
library(dismo)
library(RgoogleMaps)

graph_path <- "../../DOC/GRAPHS/"

##################################
## Map data on google map tiles ##
##################################
                                        # Plot currency chests
plot_india_map_cc <- function(){
    lat = c(35.674520, 6.74678, 28.01744, 23.71307)
    lon = c(76.845245, 93.84260, 97.40238, 68.03215)
    center = c(mean(lat), mean(lon));
    zoom <- min(MaxZoom(range(lat), range(lon)));
    newmap <- GetMap(
        center = center,
        zoom = zoom,
        destfile = paste0(graph_path, "india_cc_map.png"), 
        maptype = "mobile")
    png(paste0(graph_path, "india_cc_map.png"))
    PlotOnStaticMap(newmap,
                    lat = cc_latlon$lat, 
                    lon = cc_latlon$lon, 
                    destfile = paste0(graph_path, "india_cc_map.png"),
                    cex = 1,
                    pch = 8,
                    col = 'gold',
                    add = FALSE)
    dev.off()
}

                                        # Plot bank branches
plot_india_map_pin_dir <- function(){
    lat = c(35.674520, 6.74678, 28.01744, 23.71307)
    lon = c(76.845245, 93.84260, 97.40238, 68.03215)
    center = c(mean(lat), mean(lon));
    zoom <- min(MaxZoom(range(lat), range(lon)));
    newmap <- GetMap(
        center = center,
        zoom = zoom,
        destfile = paste0(graph_path, "india_pin_map.png"),
        maptype = "mobile")
    png(paste0(graph_path, "india_pin_map.png"))
    PlotOnStaticMap(newmap,
                    lat = pin_latlon$lat, 
                    lon = pin_latlon$lon, 
                    destfile = paste0(graph_path, "india_pin_map.png"),
                    cex = 1,
                    pch = 8,
                    col = 'darkblue',
                    add = FALSE)
    dev.off()
}
