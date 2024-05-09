library(sf)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(shadowtext)

load(file.choose())#map.RData

# rv3 <- read.xlsx(file.choose())
# shp <- taiwanmap[c(3,5)]#台灣地圖
# rv <- st_read(file.choose())



map_plot_result <- function(data, river_no) {
  colors <- c("blue","blueviolet","brown","chartreuse","chocolate",
              "cornflowerblue","coral","cornsilk4","darkgoldenrod1",
              "darkgreen","darkmagenta","darkolivegreen1","darksalmon",
              "darkslategray","deeppink","gold","firebrick1","forestgreen",
              "deeppink4","khaki1","sienna4","black","seagreen1","violet",
              "yellowgreen","rosybrown1")
  id <- list()
  
  for(i in river_no) {
    id[[i]] <- match(data[[i]], data_l$locationID)
  }
  
  all_id <- unlist(id)
  data_loc <- data_l[all_id,]  
  data_loc <- na.omit(data_loc)
  
  data_loc$BA_NO <- substr(data_loc$locationID, 1, 4)
  merged_data <<- merge(data_loc, rv3, by.x = "BA_NO", by.y = "BA_NO")
  
  
  lat <- data_loc$decimalLatitude
  lon <- data_loc$decimalLongitude
  
  multi_lines <- st_cast(rv[,5], "MULTILINESTRING")
  rvline <- st_cast(multi_lines, "LINESTRING")
  
  p <- ggplot() + 
    geom_sf(data = shp, color = "black") + 
    geom_sf(data = rvline, color = "skyblue", size = 5) +
    theme_void()
  
  sf <- st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326)
  bbox <- st_bbox(sf)
  current_data1 <<- data.frame()
  if (length(river_no) == 26) {
    p <- p + coord_sf(xlim = c(120.0612, 121.95), ylim = c(22.03, 25.3))
    for (i in river_no) {
      current_id <- id[[i]]
      current_data <- data_l[current_id,]
      current_data <- na.omit(current_data)
      current_data$label <- i
      
      current_data1 <<- rbind(current_data1, current_data)
        
      
      # p <- p + geom_point(data = current_data, aes(x = decimalLongitude, y = decimalLatitude), 
      #                     color = colors[i], size = 3, na.rm = TRUE)
    }
  } else if (length(river_no) == 1) {
    p <- p + coord_sf(xlim = c(0.9995 * bbox["xmin"], 1.0002 * bbox["xmax"]), 
                      ylim = c(0.9995 * bbox["ymin"], 1.0002 * bbox["ymax"])) + 
      geom_shadowtext(data = data_loc, aes(x = decimalLongitude, y = decimalLatitude, label = locality), 
                      size = 7, color = "white", 
                      fontface = "bold", vjust = 2)
    for (i in river_no) {
      current_id <- id[[i]]
      current_data <- data_l[current_id,]
      current_data <- na.omit(current_data)
      
      p <- p + geom_point(data = current_data, aes(x = decimalLongitude, y = decimalLatitude), 
                          color = "red", size = 7, na.rm = TRUE)
    }
  } else {
    p <- p + coord_sf(xlim = c(0.9995 * bbox["xmin"], 1.0002 * bbox["xmax"]), 
                      ylim = c(0.9995 * bbox["ymin"], 1.0002 * bbox["ymax"]))
    for (i in river_no) {
      current_id <- id[[i]]
      current_data <- data_l[current_id,]
      current_data <- na.omit(current_data)
      
      p <- p + geom_point(data = current_data, aes(x = decimalLongitude, y = decimalLatitude), 
                          color = colors[i], size = 7, na.rm = TRUE)
    }
  }
  
  
  
  return(p)
}



##### result #####

####取最好的
# result_fish1 <- result_fish
# for (i in c(4,5,7,9,10,12,15,19,21,22,23,24,25,26)) {
#   result_fish1[[i]] <- unlist(result_fish[[i]][1])
# }

map_plot_result(result_fish1,1:26)

save(data,data_l,data_r,result_fish,result_fish1,rv2,rv3,rv,taiwanmap,shp,size,file = "map.RData")
sf_data <- st_as_sf(current_data1, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
st_write(sf_data, "fish.geojson", encoding = "UTF-8")

