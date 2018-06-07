
plotStackedBar <- function(data, groupVar, colorVar){
  
  if(!isTruthy(groupVar) & !isTruthy(colorVar)){
   
    print("Missing Group Var or Color Var") 
    
  } else {
    
    plot <- ggplot(data = data, aes_(x = as.name(groupVar), fill = as.name(colorVar))) + geom_bar(position = "fill") + scale_y_continuous(labels = scales::percent)
    
    return(plot)
    
  }

}