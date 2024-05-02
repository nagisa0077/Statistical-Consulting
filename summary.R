library(iNEXT)
load("Incidence_data.RData")
load("map.RData")
category_list <- data.frame(NO = c(1:11),category = c("1 魚類","2 蝦蟹類","3 昆蟲類",
                                                      "4 螺貝類","5 環節動物","6 藻類",
                                                      "7 植物","8 哺乳類"," 鳥類",
                                                      "10 爬蟲類","11 兩棲類"))

summary <- function(category, river){
  
}


category = 2
river = 3

river_name <-  rv3$BA_NO[which(rv3$NO == river)]
category_name <- category_list$category[which(category_list$NO == category)]

incidence_dataset[[river]][[category]]

# 估計

a <- ChaoRichness(incidence_dataset[[river]][[category]], datatype = "incidence_raw")
b <- ChaoRichness(incidence_dataset[[river]][[category]], datatype = "incidence_raw")
c <- bind_rows(a,b)


for