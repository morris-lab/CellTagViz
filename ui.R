


#This is the file used to define the UI for the Shiny App to explore CellTag Data.

#The definition of the UI function should be at the end of the file. All other code or commands should come before the UI function.

tagList(
  
  tags$head(includeHTML(("html/google-analytics.html"))),
  
navbarPage(
  
  "CellTag Viz",
  
  theme = shinytheme("cosmo"),
  
  tabPanel(
    
    "Welcome!",
    
    mainPanel(
      
      includeHTML("html/welcome.edit.html")
      
    )
    
    ),
  
  tabPanel(
    "Plots",
    
    sidebarPanel(
      id = "Sidebar",
      width = 3,
      
      conditionalPanel(
        condition = "input.tab == 1 || input.tab == 3",
        
        selectizeInput(
          inputId = "metaVariable",
          label = "Color Plot by:",
          choices = selectChoices,
          multiple = FALSE,
          selected = "Cluster.Seurat"
        ),
        
        
        selectizeInput(
          inputId = "geneChoice",
          label = "",
          choices = geneChoice,
          options = list(
            placeholder = "Select A Gene",
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        
        selectInput(
          inputId = "clonesD0variable",
          label = "Clones D0:",
          choices = cloneChoicesD0,
          selectize = TRUE,
          multiple = TRUE
        ),
        
        
        selectInput(
          inputId = "clonesD3variable",
          label = "Clones D3:",
          choices = cloneChoicesD3,
          selectize = TRUE,
          multiple = TRUE
        ),
        
        
        selectInput(
          inputId = "clonesD13variable",
          label = "Clones D13:",
          choices = cloneChoicesD13,
          selectize = TRUE,
          multiple = TRUE
        ),
        
        checkboxInput(
          inputId = "contour",
          label = "Add Contour",
          value = FALSE
        ),
        
        downloadButton(outputId = "downloadPlot",
                       label = "Download Plot"
        )
        
      ),
      
      conditionalPanel(
        condition = "input.tab == 4",
        
        selectInput(
          inputId = "groupVariable",
          label = "Group By:",
          choices = selectChoices[-c(1,2,4,6, 13)],
          selectize = TRUE,
          multiple = FALSE,
          selected = "replicate"
        ),
        
        selectInput(
          inputId = "stackedVariable",
          label = "Color By:",
          choices = selectChoices[-c(1,2,4,6, 13)],
          selectize = TRUE,
          multiple = FALSE,
          selected = "Cluster.Seurat"
        ),
        
        downloadButton(outputId = "downloadStackedPlot",
                       label = "Download Plot"
        )
        
      ),
      
      conditionalPanel(
        condition = "input.tab == 2",
        
        selectInput("rep", "Select a Replicate", choices = c("HF1", "HF2")),
        selectInput("tag", "Select tag to Plot", choices = all_tags),
        selectInput("c", "Select colors to overlay", choices = c("cluster", "tag", "SuperClone"))
        
        
    )
      
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tab",
        
        type = "pills",
        
        tabPanel("tSNE", plotOutput("tSNEplot"), value = 1),
        
        tabPanel("Clone Networks", forceNetworkOutput("cloneNetwork"), value = 2),
        
        tabPanel("Pseudotime", plotOutput("pseudotime"), value = 3),
        
        tabPanel("Stacked Bar Chart", plotOutput("StackedBarChart"), value = 4),
        
        tabPanel("Meta Data", DT::dataTableOutput("cloneTable", width = "50%", height = "auto"), value = 5)
        
      )
      
    )
  ),
  
  tabPanel(
    
    "Data",
    
    mainPanel(
      
      shinyjs::useShinyjs(),
      
      downloadLink(outputId = "downloadCellTagData", label = "Click here to download data used to generate plots.")
      
    )
    
    ),
  
  tabPanel(
    
    "People",
    
    mainPanel(
      
      includeHTML("html/people.tab.edit.html")
      
    )
  )
  
)
)
