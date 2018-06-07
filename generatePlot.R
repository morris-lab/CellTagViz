

generatePlot <-
  function(data,
           colorChoice = "Cluster.Seurat",
           plotType = 1,
           cloneChoice = NULL,
           tagCols = NULL,
           contour = NULL,
           groupVar = NULL,
           geneExpr = NULL) {
    
    if (plotType == 4) {
      
      plotStackedBar(data = data,
                     groupVar = groupVar,
                     colorVar = colorChoice)
      
    } else{
      
      if(isTruthy(geneExpr)){
        
        data <- addGeneExpr(data, geneExpr, plotType)
        
        if(plotType == 3){
          
          colorChoice <- strsplit(x = geneExpr, split = "_")[[1]][1] 
        
        } else if(plotType == 1){
          
          colorChoice <- geneExpr
          
        }
        
      }
      
      if (plotType == 3) {
        
        cellsToUse <- data$Monocle
        
        data <- data[cellsToUse == TRUE, ]
        
      }
      
      nullCount <- lapply(cloneChoice, "is_null")
      
      nullCount <- unlist(nullCount)
      
      if (sum(nullCount) < 3) {
        
        cloneRows <-
          subsetCloneData(data, cloneChoice = cloneChoice, tagCols = celltagCols)
        
        cloneData <- data[cloneRows, ]
        
        backgroundData <- data[-cloneRows, ]
        
        cloneData <- cloneVersion(cloneData, cloneChoice)
        
        background <-
          plotBase(
            data = backgroundData,
            colorChoice = colorChoice,
            plotType = plotType,
            background = TRUE
          )
        
        clonePlot <-
          plotCloneLayer(data = cloneData,
                         colorChoice = colorChoice,
                         plotType = plotType)
        
        if (isTruthy(contour)) {
          contourLayer <-
            plotContour(data = cloneData,
                        colorChoice = colorChoice,
                        plotType = plotType)
          
          finalPlot <- background + clonePlot + contourLayer
          
          return(finalPlot)
          
        } else{
          finalPlot <- background + clonePlot
          
          return(finalPlot)
        }
        
      } else {
        if (isTruthy(contour)) {
          base <-
            plotBase(data = data,
                     colorChoice = colorChoice,
                     plotType = plotType)
          
          contourLayer <-
            plotContour(data = data,
                        colorChoice = colorChoice,
                        plotType = plotType)
          
          finalPlot <- base + contourLayer
          
          return(finalPlot)
          
        } else{
          finalPlot <-
            plotBase(data = data,
                     colorChoice = colorChoice,
                     plotType = plotType)
          
          return(finalPlot)
          
        }
        
      }
      
    }
    
  }