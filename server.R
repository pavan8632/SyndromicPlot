library(shiny)
library(gdata)
library(corrplot)

library(gridExtra)
library(png)
library(grid)
library(extrafont)
loadfonts()


source("./PCAFunc.R")
source("./FergPlot.R")

#Define server logic

shinyServer(function(input,output){
  pp<- theme(
    axis.line = element_line(colour = 'gray', size = .75), 
    panel.background = element_blank(),  
    plot.background = element_blank()
  )	
  #get the data from input window
  data<-reactive({
    dataFile <- input$file1
    if(is.null(dataFile))
      return(NULL)
    x=read.csv(dataFile$datapath, header=input$header,sep=',',quote="'",row.names=NULL)
    x=as.data.frame(x)
    if(rownames(x)[1]==1){
      rownames(x)=x[,1]
      x[,1]=NULL
      return(x)
    }
    else{
      return(x)
    }
    
  })
 
  output$FPlot<-renderUI({
    if(is.null(data()))
      return(NULL)
    sliderInput("Pc2Plot","Choose which PC to create your FPlot from",min=1,max=ncol(data()),value=1,step=1)
  })
  output$NumArr<-renderUI({
    if(is.null(data()))
      return(NULL)
    numericInput("NumArr", "Number of Maximum Arrows for Ferg Plot, Likely will be less as all numbers <.3 will be dropped",5,min=2,max=15)
    
  })
  output$CutOff<-renderUI({
    if(is.null(data())){
      return(NULL)
    }
    numericInput("cutoff","Choose a value of significance for Loadings, between 0-1",.3,min=0,max=1)
  })
  
  output$contents<-renderPrint({
    if (is.null(data()))
      return(NULL)
    else {
      data()
    }
  })


  output$downloadBplot<-downloadHandler(
    
    filename=function(){paste('FergPlot',toString(input$Pc2Plot),'.pdf',sep='')},
    content=function(file){
      
      ggsave(file, plot = BasicPlot(),width=15,height=10,units="in")
    }
  )
  
  output$varexp<-renderUI({
    if(is.null(data()))
      return(NULL)
    else{
  numericInput("varexp",'Percent Variation explained by this PC',0,min=0,max=100)
    }
  })
  
  BasicPlot<-reactive({
    if (is.null(data())) {
      return(NULL)
    } else {
      tmp<-data()
      i<-input$Pc2Plot
      varexp<-input$varexp
      pctitle<-input$pctit
      a<-LoadingSort(tmp[i],input$cutoff,input$NumArr)
      b<-FergusonPlotCoordinates(a)
    
      plot<-FergusonPlot(b,varexp,pctitle)
      plot
    }
    })
  output$pctitle<-renderUI({
    if(is.null(data()))
      return(NULL)
   
    textInput('pctit','Title for Center of Plot',value=NULL)
  })
  
  output$BPlot<-renderPlot({
    BasicPlot()
    
    
    
  })
  
  
  
})
