
addGeneExpr <- function(data, gene, plotType){
  
  if(plotType == 1){
  
    data[colnames(seuratExpr), gene] <- seuratExpr[gene, ]
    
  } else if(plotType == 3){
    
    geneID <- gene
    
    geneName <- strsplit(x = geneID, split = "_")[[1]][1]
    
    data[colnames(monocleExpr), geneName] <- log10( monocleExpr[geneID, ] + 0.1)
    
  }
  
  return(data)
  
}