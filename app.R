#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("findOptionsByStock.R")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
 
  
  verticalLayout(
    titlePanel("Options By Stock"),
       wellPanel(
      helpText("Enter Stock symbol for the Options:"),
      div(style="display:inline-block;width:50px",HTML("<br>")),
      
      div(style="display:inline-block;width:100px;vertical-align:top",textInput("symb", "Symbol", "NGL")),
      div(style="display:inline-block;width:30px",HTML("<br>")),
      div(style="display:inline-block;width:300px;vertical-align:top",dateRangeInput("dates",  "Date range", start = as.character(Sys.Date()-365),  end = as.character(Sys.Date()))),
      div(style="display:inline-block;width:50px",HTML("<br>")),
      div(style="display:inline-block;vertical-align:bottom",checkboxInput("OI",   "Filter out OI < 10", value = FALSE)), 
      div(style="display:inline-block;width:50px",HTML("<br>")),
      div(style="display:inline-block;vertical-align:bottom",checkboxInput("spread",   "Filter out High Spreads", value = FALSE)),
      
    
      submitButton("Update View")
      ),
    # Resonse Variables 
    wellPanel(strong(textOutput("stock")),
    tags$head(tags$style("#stock{color: blue; font-size: 15px;  font-style: italic; }" ) ),
  
     div(style="display:inline-block",strong(textOutput("stockPrice"))),
    div(style="display:inline-block;width:100px",HTML("<br>")),
       div(style="display:inline-block",textOutput("changeP")) , 
    div(style="display:inline-block;width:100px",HTML("<br>")),
    div(style="display:inline-block",strong(textOutput("volatility20")) )
    ),
    dataTableOutput("optionsTable")
    
    
  )
  
))

server <- shinyServer(function(input, output) {
   
  dataInput <- reactive({
    processSymbol(sym=input$symb,startDate = input$dates[1],endDate = input$dates[2])
    # fetchOptions(input$symb)
  })
  
  filterData <-function(dt){
    if(input$OI) dt=dt[OI>10,]
    if(input$spread) dt=dt[spread<0.5,]
    return(dt)
    }
  
  output$stock<-renderText(paste("Processing Symbol: ",toupper(input$symb)))
  output$stockPrice<-renderText(paste("$", round(dataInput()[.N,stockPrice],2)  ))
  output$volatility20<-renderText(paste( "Volatility:",round(100*dataInput()[1,volatility20],2),"%") )
  output$changeP<-renderText(paste( "Change Percent",dataInput()[.N,changeP],"%") )
  
  
   output$optionsTable <- renderDataTable({   
     data<-dataInput()[,.(date=as.Date(gsub("*\\D[[:digit:]]*$","",gsub(pattern="(^[[:alpha:]]+)","",OS)),format = "%y%m%d"),XP = as.numeric(gsub(pattern="(^[[:alpha:]]+)\\d{6}[[:alpha:]]","",OS))/1000,OI,marketPrice,optionPrice,delta,OptionPriceRatio,spread,OS)]
     filterData(data)
     
     })
})

# Run the application 
shinyApp(ui = ui, server = server)

