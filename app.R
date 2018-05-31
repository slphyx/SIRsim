## saralamba@gmail.com
# 30 May 2018
#####

library(shiny)
source("hatgame.R")

ui <- fluidPage(
  tags$h2("Hat Game"),
  hr(),
  
  fluidRow(
    wellPanel(
      
      fluidRow(
        column(2,NULL),
        column(4,
               
               sliderInput("npop","the number of population", min = 10,max = 100,step = 1, value = 30),
               sliderInput("R0","R0", min = 1,max = 50,step = 1,value = 5),
               sliderInput("ntimes","the number of time steps", min = 1,max = 30,step = 1,value = 10)
               
        ),
        
        column(4,
               
               sliderInput("move.radius","individual movement radius", min = 0.01, max = 0.99,step = 0.01,value = 0.3),
               sliderInput("inf.radius","infected radius", min = 0.01, max = 0.99,step = 0.01,value = 0.3),
               actionButton("runButton", "Run Simulation")
               
               ),
        
        column(2,NULL)
        )
        
      )
    ),
    
    fluidRow(
      
      column(1,NULL),
      column(7, 
             uiOutput("slider.thtime"),
             plotOutput("moves"),
             plotOutput("graphs")
             ),
      column(3, tableOutput("sir.table")),
      column(1,NULL)
      
    )
    
)

server <- function(input, output, session) {
  source("hatgame.R")
  
  bigpop <- reactive({
    input$runButton
    
    HatGame(input$npop, input$R0, input$inf.radius, input$move.radius, input$ntimes)
  })
 
  
   
  output$slider.thtime <- renderUI({
    sliderInput("slider.time", "the positions at the nth step", min = 0, max = input$ntimes, step = 1, value = 0)
  })

  output$graphs <- renderPlot({SIR.plot(bigpop())})
 
  output$sir.table <- renderTable({HatGame.summary(bigpop())})
  
  output$moves <- renderPlot({movePop.plot(bigpop(),input$slider.time)})
  
}


shinyApp(ui, server)
