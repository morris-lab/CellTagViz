
#This is a function to create basic tSNE or pseudotime plots.
#The plots are colored by the chosen meta data variable.

plotBase <- function(data, plotType, colorChoice, background = FALSE){
  
  if(plotType == 1){
    
    xDim <- "tSNE_1"
    
    yDim <- "tSNE_2"
    
    cellSize = 0.75
    
    } else if (plotType == 3){
      
      xDim <- "Component.1"
      
      yDim <- "Component.2"
      
      cellSize = 1
      
    }
  
  if(background){
    
    plot <- ggplot(data = data, aes_(x = as.name(xDim), y = as.name(yDim))) + geom_point(size = cellSize, color = "grey70", na.rm = TRUE) 
    
  } else {
    
    plot <- ggplot(data = data, aes_(x = as.name(xDim), y = as.name(yDim), color = as.name(colorChoice))) + geom_point(size = cellSize, na.rm = TRUE) 
    
  }
  
  if(!(colorChoice %in% selectChoices)){
    
    plot <- plot + scale_color_viridis()
    
  }
  
  
  return(plot)
  
}

