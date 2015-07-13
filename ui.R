library(shiny)
source("./PCAFunc.R")
shinyUI(pageWithSidebar( 
  headerPanel("Principal Component Analysis"),
  
  sidebarPanel(
    fileInput('file1','Choose PC Loadings CSV File', accept=c('text/csv,','.csv')),
    checkboxInput('header', 'Header', TRUE),
    uiOutput("FPlot"),
    uiOutput("CutOff"),
    uiOutput("NumArr"),
    uiOutput("varexp"),
    uiOutput('pctitle')
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data",tableOutput("contents")),
      tabPanel("Ferg Plot",plotOutput("BPlot"),downloadButton('downloadBplot','Download this plot'))
    ) 
  )
  
  
  
  
  
))
