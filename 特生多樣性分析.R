########多樣性分析#########
load(file.choose()) #開啟Incidence_data.RData
library(iNEXT)

#轉成row_sum格式
row_sum <- list()
for (i in names(incidence_dataset)) {
  a <- lapply(incidence_dataset[[i]],FUN = as.incfreq)
  row_sum[[i]] <- a
}
View(row_sum)

#稀釋曲線
l <- iNEXT(row_sum[[2]][[1]], datatype = "incidence_freq")
ggiNEXT(l, type = 2)


######max.chao2
max.chao2 <- function(data){
  coln <- colnames(data)
  all_combinations <- combn(coln, 15)
  chao <- list()
  
  process_combination <- function(comb) {
    
    input <- data[,comb]
    chao2 <- ChaoRichness(input,datatype = "incidence_raw")
    chao <- list(chao2,comb)
    
  }
  
  for (i in 1:ncol(all_combinations)) {
    chao[[i]] <- process_combination(all_combinations[, i])
  }
  
  # 創建一個向量來儲存每個計算結果的第二個子列表中的第二個元素
  chao_values <- sapply(chao, function(x) x[[1]][[2]])
  top5_indices <- order(chao_values, decreasing = TRUE)[1:5]
  top5_chao_values <- chao_values[top5_indices]
  top5_chao_combinations <- chao[top5_indices]
  print("前五個最大值及其對應的組合:")
  for (i in 1:5) {
    print(paste0("Top",i))
    print(top5_chao_combinations[[i]])
  }
  # 找到最大值及其索引位置
  #max_chao_index <- which.max(chao_values)
  #max_chao_combination <- chao[[max_chao_index]][[2]]
  #max_chao_value <- chao_values[max_chao_index]
  
  # 最大值及其對應的組合
  ##print(paste("最大值:", max_chao_value))
  #print(paste(max_chao_index))
  #print(paste0( max_chao_combination))
  
}

##example
max.chao2(incidence_dataset[[2]][[1]])
load(file.choose())
