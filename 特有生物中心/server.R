
library(shiny)
library(sf)
library(ggplot2)
library(writexl)
library(dplyr)
library(shadowtext)

# 載入地圖資料
load("map.RData")


shp <- taiwanmap[c(3, 5)]  # 台灣地圖

# 定義地圖繪製函數
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
    geom_sf(data = shp, color = "black", fill = "palegreen4") + 
    geom_sf(data = rvline, color = "skyblue", size = 3) +
    theme_void()
  
  sf <- st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326)
  bbox <- st_bbox(sf)
  
  if (length(river_no) == 26) {
    p <- p + coord_sf(xlim = c(120.0612, 121.95), ylim = c(22.03, 25.3))
    for (i in river_no) {
      current_id <- id[[i]]
      current_data <- data_l[current_id,]
      current_data <- na.omit(current_data)
      
      p <- p + geom_point(data = current_data, aes(x = decimalLongitude, y = decimalLatitude), 
                          color = colors[i], size = 3, na.rm = TRUE)
    }
  } else if (length(river_no) == 1) {
    p <- p + coord_sf(xlim = c(0.9995 * bbox["xmin"], 1.0002 * bbox["xmax"]), 
                      ylim = c(0.9995 * bbox["ymin"], 1.0002 * bbox["ymax"])) + 
      geom_shadowtext(data = data_loc, aes(x = decimalLongitude, y = decimalLatitude, label = locality), 
                      size = 4, color = "white", 
                      fontface = "bold", vjust = 2)
    for (i in river_no) {
      current_id <- id[[i]]
      current_data <- data_l[current_id,]
      current_data <- na.omit(current_data)
      
      p <- p + geom_point(data = current_data, aes(x = decimalLongitude, y = decimalLatitude), 
                          color = "red", size = 3, na.rm = TRUE)
    }
  } else {
    p <- p + coord_sf(xlim = c(0.9995 * bbox["xmin"], 1.0002 * bbox["xmax"]), 
                      ylim = c(0.9995 * bbox["ymin"], 1.0002 * bbox["ymax"]))
    for (i in river_no) {
      current_id <- id[[i]]
      current_data <- data_l[current_id,]
      current_data <- na.omit(current_data)
      
      p <- p + geom_point(data = current_data, aes(x = decimalLongitude, y = decimalLatitude), 
                          color = colors[i], size = 3, na.rm = TRUE)
    }
  }
  
  
  
  return(p)
}



shinyServer(function(input, output, session) {

  
  btn_ids <- paste0("btn_", 1:26)
  
  
  lapply(1:26, function(i) {
    observeEvent(input[[paste0("btn_", i)]], {
      river_no <- i
      output$river_plot <- renderPlot({
        map_plot_result(result_fish1, river_no)
      })
      
      output$river_info <- renderUI({
        station_names <- data.frame()
        river_name <- rv3$RV_NAME[which(rv3$NO == river_no)]
        station_names <- merged_data$locality
        info_text <- paste0(
          "<div style='font-size: 22px; text-align: center;'>",
          "<b>",river_name,"Station Information","</b>", "<br>","<br>",
          "<div style='text-align: left; font-size: 16px;'>",
          "<b>Stations: </b>","<br>", paste(station_names, collapse = ", "),
          "</div>",
          "</div>"
        )
        HTML(info_text)
      })
    })
  })
  
  
})



