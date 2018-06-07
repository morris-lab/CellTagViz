
plot.tsne <- function(data, colorVar){
  
  ggplot() + geom_point(data = data, aes_(x = ~tSNE_1, y = ~tSNE_2, color = as.name(colorVar)) , size = 0.75)
  
}

plot.tsne.contour <- function(data, colorVar){
  
  #tsne.plot <- plot.tsne(data, colorVar)
  
  stat_density2d(data = data, aes_(x = ~tSNE_1, y = ~tSNE_2, color = as.name(colorVar)))
}

plot.tsne.background <- function(data){
  
  geom_point(data = data, aes_(x = ~tSNE_1, y = ~tSNE_2), size = 0.75, color = "grey70")
  
}

plot.tsne.clone <- function(data, cloneVar, colorVar, contourVar = NULL){
  
  cells.to.use <- data$v1.1 %in% cloneVar
  
  base <- ggplot() + geom_point(data = data[!cells.to.use, ], aes_(x = ~tSNE_1, y = ~tSNE_2), color = "grey70", size = 0.75)

  clone.layer <- geom_point(data = data[cells.to.use, ], aes_(x = ~tSNE_1, y = ~tSNE_2, color = as.name(colorVar)), size = 0.75)
    
  if(isTruthy(contourVar)){
    
    contour.layer <- plot.tsne.contour(data[cells.to.use, ], colorVar)
    
    base + clone.layer + contour.layer
  
    } else {
      
      base + clone.layer
    
  }
  
  
}
