library(iNEXT)
load("Incidence_data.RData")
load("map.RData")
category_list <- data.frame(NO = c(1:11),category = c("1 魚類","2 蝦蟹類","3 昆蟲(含水生昆蟲)",
                                                      "4 螺貝類","5 環節動物","6 藻類",
                                                      "7 植物","8 哺乳類","9 鳥類",
                                                      "10 爬蟲類","11 兩棲類"))

summary <- function(category, river){
  river_name <-  rv3$BA_NO[which(rv3$NO == river)]
  category_name <- category_list$category[which(category_list$NO == category)]
  
  if (category_name %in% names(incidence_dataset[[river_name]])) {
    result <- ChaoRichness(incidence_dataset[[river_name]][[category_name]], datatype = "incidence_raw")
    return(
      data.frame(river_name = river_name, category_name = category_name, t = ncol(incidence_dataset[[river_name]][[category_name]]), Chao_Richness = result)
    )
  } else {
    return(NULL)
  }
  
}

category = 1
river = 1


df <- data.frame()
for(category in 1:11){
  for(river in 1:26){
    df <- rbind(df, summary(category, river))
  }
}

summary(1, 2)
summary(3, 2)
