library(shiny)
library(sf)
library(ggplot2)
library(writexl)
library(dplyr)
library(shadowtext)

####載入地圖資料
load("map.RData")


shp <- taiwanmap[c(3, 5)]  #臺灣地圖

####將結果印出來(圖片、樣站)
map_plot_result <- function(data, river_no) {
  
  id <- list()
  
  for(i in river_no) {
    id[[i]] <- match(data[[i]], data_l$locationID)
  }
  
  all_id <- unlist(id)
  data_loc <- data_l[all_id,]  
  data_loc <- na.omit(data_loc)
  
  data_loc$BA_NO <- substr(data_loc$locationID, 1, 4)
  merged_data <- merge(data_loc, rv3, by.x = "BA_NO", by.y = "BA_NO")

  return(merged_data)
}
  
shinyServer(function(input, output, session) {

  ####當按按鈕時印出結果
  lapply(1:26, function(i) {
    
    observeEvent(input[[paste0("btn_", i)]], {
      
      
      img <- img(src = paste0(i,".jpeg"), width = "100%", height = "100%",
                 style = "box-shadow: 4px 4px 4px #A3D1D1; border: 2px solid 		#81C0C0;")
      
      
      output$river_info <- renderUI({
        merged_data <- map_plot_result(result_fish1,i)
        station_names <- data.frame()
        river_name <- rv3$RV_NAME[which(rv3$NO == i)]
        station_names <- merged_data$locality
        info_text <- paste0(
          "<div style='font-size: 22px; text-align: center; font-weight: bold;
          color: black;text-shadow: -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff;'>",
          "<b>",river_name,"樣站資訊","</b>", "<br>","<br>",
          
          "<div style='font-size: 18px;  font-weight: bold; text-align: left;
          color: black;text-shadow: -1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff;'>",
          "<b>挑選樣站: </b>","<br>",
          
          "<div style='text-align: left; font-size: 16px; font-weight: bold;
          color: white;text-shadow: 1px 1px 2px #000;'>",
           paste(station_names, collapse = "、 "),"<br>",
          "</div>",
          "</div>",
          "</div>",
          img 
        )
        HTML(info_text)
      })
      
      
    })
  })
  
  
})



