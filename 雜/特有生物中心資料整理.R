library(openxlsx)
##載入特生原始資料csv、物種清單
raw <- read.xlsx(file.choose())#特生原始資料
data <- raw[,c(1,4,7,10,11,12,13,15,16,19,20)]#提取重要變數
data <- subset(data, !is.na(decimalLatitude))
specieslist <- read.xlsx(file.choose())#物種清單


species <- specieslist[,c(1,2,4)]#提取taxonGroup、taxonUUID、scientificName
names(species)[names(species) == "taxonUUID"] <- "taxonID" #把species的taxonUUID改名成taxonID
databytaxon <- merge(data, species, by = "taxonID", all = F) #透過taxonID合併兩資料


####確認合併後的scientificName跟taxonID個數相同
length(unique(databytaxon$scientificName))#學名個數
length(unique(databytaxon$taxonID))#物種ID個數

loclass <- substr(databytaxon$locationID, 1, 4) #locationID前四碼共26個(不包括NA)
dataset <- split(databytaxon, loclass)#透過前四碼將資料分為26條流域




####迴圈將26個流域中再依物種類群分
grouped_dataset <- list() #建立空列表

for (loc in names(dataset)) { #從各個流域跑迴圈
  
  loc_data <- dataset[[loc]] #取得當前流域名
 
  unique_taxon_groups <- unique(loc_data$taxonGroup) #取得當前流域所有的物種類群名
  
  
  grouped_data_list <- list() #空列表用於存分割後的資料
  
  ###在當前類群裡作迴圈
  for (group_name in unique_taxon_groups) {
    
    group_subset_data <- subset(loc_data, taxonGroup == group_name)#依類群名分群
    
    grouped_data_list[[group_name]] <- group_subset_data #將分好的子集存到列表
  }
  
  grouped_dataset[[loc]] <- grouped_data_list #將當前列表存至dataset
}


View(grouped_dataset)###流域、物種合併後資料



######將資料轉成incidence資料
incidence_dataset <- list()#建立空列表

#流域、物種合併後資料中各流域資料做迴圈
for (loc in names(grouped_dataset)){
  
  river <- list() #流域空列表
  
  for(group_name in names(grouped_dataset[[loc]])){ #各流域中各類群資料做迴圈
    #物種名清單
    species_names <- unique(grouped_dataset[[loc]][[group_name]]["scientificName"])$scientificName
    
    #測站名清單
    station_names <- unique(grouped_dataset[[loc]][[group_name]]["locationID"])$locationID
    
    #建立空incidence matrix
    observation_matrix <- matrix(0, nrow = length(species_names), ncol = length(station_names))
    
    rownames(observation_matrix) <- species_names #建立matrix row名
    colnames(observation_matrix) <- station_names #建立matrix column名
    
    for (i in 1:nrow(grouped_dataset[[loc]][[group_name]])){ #每筆資料做迴圈
      #紀錄該筆資料的學名
      species <- grouped_dataset[[loc]][[group_name]]$scientificName[i]
      
      #紀錄該筆資料的測站
      station <- as.character(grouped_dataset[[loc]][[group_name]]$locationID[i])
      observation_matrix[species, station] <- 1 #填入incidence
    }
    river[[group_name]] <- observation_matrix #將該類群的matrix放入該流域
  }
  incidence_dataset[[loc]] <- river #將該流域資料放入總資料集
}

#因1650之昆蟲只有筆資料，故刪除
keep_names <- names(incidence_dataset[["1650"]]) != "3 昆蟲(含水生昆蟲)"
incidence_dataset[["1650"]] <- incidence_dataset[["1650"]][keep_names]

save(incidence_dataset, file = "Incidence_data.RData")#在當前工作目錄存成.RData
load(file.choose()) #開啟.RData


