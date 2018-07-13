
#This is the file used to define the Shiny Server Functions.

#This includes all of the functions and code necessary to generate plots.

shinyServer(function(session, input, output) {
  
  
  observeEvent(input$tab, {
    
    if(input$tab == 5){
      
      shinyjs::hide(id = "Sidebar")
      
    }
    
    else{
      
      shinyjs::show(id = "Sidebar")
      
    }
    
  })

  plotOptions <- reactive({
    
    list(colorChoice = input$metaVariable, cloneChoice = list(D0 = input$clonesD0variable, D3 = input$clonesD3variable, D13 = input$clonesD13variable), contour = input$contour, plotType = input$tab, geneExpr = input$geneChoice, groupVar = input$groupVariable, stackVar = input$stackedVariable)
    
  }) %>% debounce(millis = 1500)
  
  observeEvent(input$tab, {
    
    if(input$tab == 1 | input$tab == 6){
      
      geneChoice <- rownames(seuratExpr)
      
      geneChoice <- c(Choose = "", geneChoice)
      
      updateSelectInput(session, inputId = "geneChoice", label = "Color by Gene Expression:", choices = geneChoice )
      
    } else if(input$tab == 3){
      
      geneChoice <- rownames(monocleExpr)
      
      geneChoice <- c(Choose = "", geneChoice)
      
      updateSelectInput(session, inputId = "geneChoice", label = "Color by Gene Expression:", choices = geneChoice)
      
    }
    
  })

  
  observe({
    
    rep <- input$rep
    
    if(rep == "HF1"){
      
      all_tags <<- hf1.all_tags
      
      all_tags1 <<- hf1.all_tags1
      
      all_tags2.1 <<- hf1.all_tags2.1
      
      all_tags2.2 <<- hf1.all_tags2.2
      
      linkList <<- hf1.linkList
      
      Nodes <<- hf1.Nodes
      
      tag_info <<- hf1.tag_info
      
      
      updateSelectInput(
        session,
        "tag",
        label = "HF1 Clones:",
        choices = hf1.all_tags,
        selected = "CellTag.D0_709"
      )
      
      updateSelectInput(
        session,
        "c",
        label = "HF1 Colors",
        choices = netColorChoice,
        selected = "Cluster.Seurat"
        
      )
      
    } else if (rep == "HF2") {
      
      all_tags <<- hf2.all_tags
      
      all_tags1 <<- hf2.all_tags1
      
      all_tags2.1 <<- hf2.all_tags2.1
      
      all_tags2.2 <<- hf2.all_tags2.2
      
      linkList <<- hf2.linkList
      
      Nodes <<- hf2.Nodes
      
      tag_info <<- hf2.tag_info
      
      
      updateSelectInput(
        session,
        "tag",
        label = "HF2 Clones:",
        choices = hf2.all_tags,
        selected = "CellTag.D0_2352"
      )
      
      updateSelectInput(
        session,
        "c",
        label = "HF2 Colors",
        choices = netColorChoice,
        selected = "Cluster.Seurat"
        
      )
      
    }
    
  }) 

  
   output$tSNEplot <- renderPlot({
     
     generatePlot(
       data = celltagData,
       colorChoice = plotOptions()[["colorChoice"]],
       cloneChoice = plotOptions()[["cloneChoice"]],
       tagCols = celltagCols,
       contour = plotOptions()[["contour"]],
       plotType = plotOptions()[["plotType"]],
       geneExpr = plotOptions()[["geneExpr"]]
     )

   })


  output$pseudotime <- renderPlot({

    generatePlot(
      data = celltagData,
      colorChoice = plotOptions()[["colorChoice"]],
      cloneChoice = plotOptions()[["cloneChoice"]],
      tagCols = celltagCols,
      contour = plotOptions()[["contour"]],
      plotType = plotOptions()[["plotType"]],
      geneExpr = plotOptions()[["geneExpr"]]
    )
    
  })
  
  output$StackedBarChart <- renderPlot(
    
    generatePlot(
      data = celltagData,
      cloneChoice = plotOptions()[["cloneChoice"]],
      colorChoice = plotOptions()[["stackVar"]], 
      groupVar = plotOptions()[["groupVar"]],
      plotType = plotOptions()[["plotType"]]
      )
  )
  
  output$cloneNetwork <- renderForceNetwork({
    drawSubnet(input$tag, input$c)
    
  })
  
  output$cloneTable <- DT::renderDataTable({
    
    DT::datatable(celltagData[,c(10:12, 18:20)])
    
  })
  
  output$downloadPlot <- downloadHandler(
    
    filename = function() {
      paste0("plot-", Sys.Date(), ".pdf")
      
    },
    
    content = function(file) {
      
      ggsave(
        file = file,
        plot = generatePlot(
          data = celltagData,
          colorChoice = plotOptions()[["colorChoice"]],
          cloneChoice = plotOptions()[["cloneChoice"]],
          tagCols = celltagCols,
          contour = plotOptions()[["contour"]],
          plotType = plotOptions()[["plotType"]],
          geneExpr = plotOptions()[["geneExpr"]]
        ),
        device = "pdf",
        width = 11,
        height = 8.5,
        units = "in"
      )
      
    }
  )
  
  output$downloadStackedPlot <- downloadHandler(
    filename = function() {
      paste0("stackedBarPlot", Sys.Date(), ".pdf")
      
    },
    
    content = function(file) {
      ggsave(
        file = file,
        plot = generatePlot(
          data = celltagData,
          colorChoice = plotOptions()[["colorChoice"]],
          groupVar = plotOptions()[["groupVar"]],
          plotType = plotOptions()[["plotType"]]
        ),
        device = "pdf",
        width = 11,
        height = 8.5,
        units = "in"
      )
      
    }
  )
  
  
  output$downloadCellTagData <- downloadHandler(
    
    filename = "morrislab.celltag.dataset.csv",
    
    content = function(file) {
      
      write.csv(x = celltagData, file = file, row.names = TRUE)
      
    },
    
    contentType = "text/csv"
    
  )
  
})