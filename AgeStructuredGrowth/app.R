

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
  
  
  #LAA
  laa<-reactive(input$linf*(1-exp(-0.5*((seq(from=1, to=input$nages, by=1))-0.01))))
  
  waa<-reactive((0.001*laa()^input$b)/100)

  maa<-reactive(1/(1+exp(-0.5*(laa()-input$l50))))
  #plot(maa~seq(from=1, to=nages, by=1))
  
  Mtrend<-0.2*(laa()/max(laa()))^-1
  
  #lorenzen
  #M<-matrix(rep(Mtrend, each=nyears), nyears,nages)
  M<-matrix(0.2, nyears,input$nages)
  N<-matrix(NA, nyears,input$nages)
  B<-matrix(NA, nyears,input$nages)
  F<-matrix(NA, nyears,input$nages)
  Z<-matrix(NA, nyears,input$nages)
  R<-rep(NA, nyears)
  SSB<-rep(NA, nyears)
  
  starting_N<-100
  N[1,1]<-starting_N
  for(i in 2:nages){
    N[1,i]<-N[1,i-1]*exp(-0.2)
  }

  B[1,]<-N[1,]*waa()
  SSB[1]<-sum(B[1,]*0.5*maa())
  
  Fsel<-ifelse(maa()<0.5, 0,1)
  
  Ftrend<-rep(NA, nyears)
  incF<-round(nyears*0.67,0)
  Ftrend[1:incF]<-seq(from=0, to=0.2, length=incF-1)
  Ftrend[(incF+1):nyears]<-0.2
  
  
  for(i in 1:nyears){
    F[i,] <- Ftrend[i]*Fsel
  }
  
  for(y in 1:nyears){
    for(a in 1:nages){
      if(Fsel[a]>0){
        F[y,a]<-abs(F[y,a]+rnorm(1,0,0.1))
      } 
    }
  }
  
  
  
  for(y in 2:nyears){
    R[y]<-abs((1*SSB[y-1])/(1+0.005*SSB[y-1])+rnorm(1,0,10))
    N[y,1]<-R[y]
    B[y,1]<-N[y,1] * waa()[1]
    
    for(a in 2:input$nages){
      N[y,a]<-N[y-1,a-1]*exp(-(M[y-1,a-1]+F[y-1,a-1]))
      B[y,a]<-N[y,a] * waa()[a]
    }
    SSB[y]<-sum(B[y,]*maa()*0.5)
  }

  

  
  
  
  
  output$SSBplot <- renderPlot({
    
  plot(SSB~years, type="l")
    
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
