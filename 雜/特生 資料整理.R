library(dplyr)
data <- data.frame(select(raw, occurrenceID, scientificName, organismQuantity, 
               eventDate, year, month, day, locationID, locality, decimalLatitude, decimalLongitude, samplingProtocol))

##### 河川代碼 #####
river <-  substr(data$locationID, 1,4)
length(table(river))
# river_names = names(table(river))
# 分成26個河川資料
data_r <-  data.frame(data,river)
list_r <-  split(data, river)

##### Locality #####
data_l <- data.frame(select(data, locationID, locality, decimalLatitude, decimalLongitude))
loc_ID <- match(unique(data_l$locationID), data_l$locationID)
data_l <- data_l[loc_ID,]
which(is.na(data_l$locality))
which(is.na(data_l$decimalLatitude))
which(is.na(data_l$decimalLongitude))

data_l <- data_l[-which(is.na(data_l$decimalLatitude)),]

rm(loc_ID,river)

# 刪除缺值資料 row
which(is.na(data$locality))


