
#This is a function to create basic tSNE or pseudotime plots.
#The plots are colored by the chosen meta data variable.

plotCloneLayer <- function(data, plotType, colorChoice){
  
  if(plotType == 1){
    
    xDim <- "tSNE_1"
    
    yDim <- "tSNE_2"
    
    cellSize = 0.75
    
  } else if (plotType == 3){
    
    xDim <- "Component.1"
    
    yDim <- "Component.2"
    
    cellSize = 1
    
  }
  
  layer <- geom_point(data = data, aes_(x = as.name(xDim), y = as.name(yDim), color = as.name(colorChoice)), size = cellSize, na.rm = TRUE) 
  
  return(layer)
  
}

