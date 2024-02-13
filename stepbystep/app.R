

library(shiny)
library(ggplot2)

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Age-Structured Population Growth"),
  
  #Text and equations 
  withMathJax(),
  helpText('Exponential population growth is defined by the equation $$f(N)=N_0(1+r)^t$$'),
  helpText('\\(N_0\\) is the initial population size'),
  helpText('\\(r\\) is the rate of growth'),
  helpText('\\(t\\) is the number of time steps'),
  
  helpText('This growth is also defined using the differential equation: $$\\frac{dN}{dt}=rN$$'),
  helpText('\\(\\frac{dN}{dt}\\) is the population growth rate'),
  helpText('\\(r\\) is still the rate of growth'),
  helpText('\\(N\\) is the population size'),
  helpText('Regardless, both equations, produce the same output:'),
  # Sidebar with a slider input 
  sidebarLayout(
    sidebarPanel(
      numericInput("nages",
                   "Number of ages:",
                   value = 15,
                   min= 1,
                   max = 100),
      numericInput("linf",
                   "L infinity (cm):",
                   min = 1,
                   max = 500,
                   value = 100),
      numericInput("l50",
                   "Length at 50% maturity (cm)",
                   min = 1,
                   max = 500,
                   value = 90),
      numericInput("b",
                   "Allometric growth parameter (b)",
                   min = 2,
                   max = 4,
                   value = 3)
    ),
    
    mainPanel(
      plotOutput("SSBplot")
    )
  )
  # mainPanel(
  #   plotOutput("diffexpgrowthPlot")
  # )
)

# Define server logic 
server <- function(input, output) {
  
  yrs<-seq(from=2000, to=2050)
  nyears<-length(yrs)
  nages<-reactive(input$nages)
  
  ages<-reactive(seq(from=1, to=input$nages, by=1))
  laa<-reactive(input$linf*(1-exp(-0.5*(ages()-0.01))))
  waa<-reactive((0.001*laa()^input$b)/100)
  maa<-reactive(1/(1+exp(-0.5*(laa()-input$l50))))
  #Mtrend<-reactive(0.2*(laa()/max(laa()))^-1)
  
  M<-reactive(matrix(0.2, nyears,input$nages))
  N<-reactive(matrix(NA, nyears,input$nages))
  B<-reactive(matrix(NA, nyears,input$nages))
  F<-reactive(matrix(NA, nyears,input$nages))
  Z<-reactive(matrix(NA, nyears,input$nages))
  R<-rep(NA, nyears)
  SSB<-rep(NA, nyears)
  
  starting_N<-100
  isolate(N())[1,1]<-starting_N
  # for(i in 2:nages()){
  #   N()[1,i]<-N()[1,i-1]*exp(-0.2)
  # }
  
  output$SSBplot <- renderPlot({
    
    plot(N()[1,]~ages(), type="l")
    
  }) 
}



# Run the application 
shinyApp(ui = ui, server = server)