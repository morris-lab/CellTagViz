
subsetCloneData <- function(data, cloneChoice, tagCols = celltag.cols){
  
 cloneDat <- data[, tagCols]

 n <- length(cloneChoice)

 cloneRows <- lapply(1:n, FUN = function(x){

   which(cloneDat[[x]] %in% cloneChoice[[x]])

 })
 
 cloneRows <- unlist(cloneRows)
 
 return(cloneRows)

}
