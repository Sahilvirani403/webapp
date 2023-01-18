#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Plot your data in the Graphs"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("file1","Upload .CSV file only"),
      selectInput("selectedcol1","select attribiutes:",
                  choices=c("A","B","C"),selected=c("A","B","C"),multiple=T)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("sel_attb_ui"),
      br(),
      tabsetPanel(
        tabPanel("DATA TABLE", 
                 br(),
                 dataTableOutput("dto")),
        tabPanel("SUMMARY", 
                 verbatimTextOutput("sel_vec")),
        tabPanel("HISTOGRAM",
                 uiOutput("selected_attrb_hist"),
                 plotOutput("histo1")
                 ),
        tabPanel("BOXPLOT",
                 uiOutput("selected_attrb_boxplotX"),
                 uiOutput("selected_attrb_boxplotY"),
                 plotOutput("boxplot"))
      )
      
      #plotOutput("distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #dfo <- read.csv2("NBA 2018-19 Season.csv",stringsAsFactors = T,sep = ",")
  dfo <- reactive(read.csv2( input$file1$datapath,stringsAsFactors = T,sep = ","))
  
  df <- reactive(dfo()[,input$selected_col])
  output$dto <-  renderDT({
    datatable(df(), filter = "top")
  })
  
  output$dto2 <-  renderDataTable({
    df
  })
  
  output$sel_attb_ui <-  renderUI({
    
    selectInput("selected_col","Select attributes :: ",
                choices = names(dfo()),selected = names(dfo()),
                multiple = T, width = "100%")
  })
  
  output$sel_vec <- renderPrint({
    input$selected_col
    summary(df())
  })
  
  output$selected_attrb_hist <- renderUI({
    selectInput("hist_attb","Select Attributes for histogram",
                choices =  names(select_if(df(),is.numeric)),multiple = F)
  })
  
  output$histo1 <- renderPlot({
    ggplot()+geom_histogram(aes(x=df()[,input$hist_attb])) + 
                              labs(x= input$hist_attb)
  })
  
  output$selected_attrb_boxplotX <- renderUI({
    selectInput("box_attbX","Select Attributes X",
                choices = names(select_if(df(),is.factor)),multiple = F)
    
  })
  
  output$selected_attrb_boxplotY <- renderUI({
    selectInput("box_attbY","Select Attributes Y",
                choices = names(select_if(df(),is.numeric)),multiple = F)
    
  })
  
  output$boxplot <- renderPlot({
    ggplot()+geom_boxplot(aes(y=df()[,input$box_attbY],
                          x=df()[,input$box_attbX]))+
                              labs(x=input$box_attbX, y=input$box_attbY )
                              
    
  })
  
  #---------------------------------------------------    
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  #---------------------------------------------------    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
