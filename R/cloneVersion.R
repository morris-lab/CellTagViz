
cloneVersion <- function(data, cloneChoice){
  
  data$CellTag.Version[data$CellTag.D0 %in% cloneChoice$D0] <- "Day 0"
  
  data$CellTag.Version[data$CellTag.D3 %in% cloneChoice$D3] <- "Day 3"
  
  data$CellTag.Version[data$CellTag.D13 %in% cloneChoice$D13] <- "Day 13"
  
  data$CellTag.Version <- as.factor(data$CellTag.Version)
  
  data$CellTag.Version <- factor(data$CellTag.Version, levels = c("Day 0", "Day 3", "Day 13"))
  
  return(data)
  
}