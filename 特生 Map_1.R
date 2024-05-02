#####
load(file.choose())
library(RgoogleMaps)

my.lat <- data_l$decimalLatitude
my.lon <- data_l$decimalLongitude

bb = qbbox(my.lat, my.lon) # 範圍
print(bb)

MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "my.png", maptype = "roadmap")
My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
tmp <- PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], destfile
                         = "my.png", cex=2.5, pch=20, col=1:4, add=F)

##### map for incidence_dataset #####
map_plot_river <- function(data){
  id <- match(unique(data$locationID), data$locationID) # 該流域所有採樣點
  data_loc <- data[id,]
  
  # 提出經緯度
  my.lat <- data_loc$decimalLatitude
  my.lon <- data_loc$decimalLongitude
  
  bb <-  qbbox(my.lat, my.lon) # 範圍
  
  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "my.png", maptype = "roadmap")
  My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
  tmp <- PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], destfile
                         = "my.png", cex=2.5, pch=20, col=1:100, add=F)
}

map_plot_incidence_dataset <- function(data){
  id <- match(colnames(data), data_l$locationID) # 該流域所有採樣點
  data_loc <- data_l[id,]
  
  # 提出經緯度
  my.lat <- data_loc$decimalLatitude
  my.lon <- data_loc$decimalLongitude
  
  bb <-  qbbox(my.lat, my.lon) # 範圍
  
  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "my.png", maptype = "roadmap")
  My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
  tmp <- PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], destfile
                         = "my.png", cex=2.5, pch=20, col=1:100, add=F)
}

map_plot <- function(data){
  id <- match(data, data_l$locationID) # 該流域所有採樣點
  data_loc <- data_l[id,]
  
  # 提出經緯度
  my.lat <- data_loc$decimalLatitude
  my.lon <- data_loc$decimalLongitude
  
  bb <-  qbbox(my.lat, my.lon) # 範圍
  
  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "my.png", maptype = "roadmap")
  My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
  tmp <- PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], destfile
                         = "my.png", cex=2.5, pch=20, col=1:100, add=F)
}

##### result #####
data <- data.frame(list_r[[9]]) # 河川list (list_r[[i]])
map_plot_river(data)

data <- incidence_dataset[[18]][[7]]
map_plot_incidence_dataset(data)

data <- result_fish[[2]]
map_plot(data)
