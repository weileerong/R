library(shiny)

ui <- fluidPage(
  titlePanel("My first Shiny!"),
  
  sidebarLayout(
    sidebarPanel (
      sliderInput(inputId = "bins",
                   label = "number of bins",
                   min=1,
                   max=5,
                   value=2
                   )
    ),
    
    mainPanel (
      plotOutput(outputId = "distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    x = faithful$eruptions
    bins = seq(min(x),max(x),length.out=input$bins + 1)
    
    hist(x,breaks=bins,col="pink",border = "white",
         xlab="Eruptions",main="Histogram of Eruption")
  })
}


shinyApp(ui = ui, server = server)
