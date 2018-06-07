
#This function generates a contour layer to add to tsne or pseudotime plots.
#The color of the contour lines is defined by the given meta data variable defined by colorChoice.

plotContour <- function(data, colorChoice, plotType){
  
  if(plotType == 1){
    
    xDim <- "tSNE_1"
    
    yDim <- "tSNE_2"
    
  } else if(plotType == 3){
    
    xDim <- "Component.1"
    
    yDim <- "Component.2"
    
  }
  
  contourLayer <- stat_density2d(data = data, aes_(x = as.name(xDim), y = as.name(yDim), color = as.name(colorChoice)), na.rm = TRUE)
  
  return(contourLayer)
  
}
