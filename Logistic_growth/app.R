
library(shiny)
library(ggplot2)
library(patchwork)
library(deSolve)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Logistic Growth"),
  #Text and equations
  withMathJax(),
  helpText('Logistic population growth is defined by the discrete equation $$f(N)=\\frac{K}{1+e^{(-r_{max}(t-t_0))}}$$'),
  helpText('\\(K\\) is the carrying capacity'),
  helpText('\\(r_{max}\\) is the maximum rate of growth, this slows as N approaches K'),
  helpText('\\(t\\) is time'),
  helpText('\\(t_0\\) is time at which the curve is at its midpoint'),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("x0",
                  "Midpoint:",
                  min = 0,
                  max = 100,
                  value = 50),
      sliderInput("r",
                  "Maximum Growth Rate:",
                  min = 0,
                  max = 0.5,
                  value = 0.3,
                  step=0.025),
      sliderInput("K",
                  "Carrying Capacity:",
                  min = 0,
                  max = 1000,
                  value = 500),
    ),
    

    mainPanel(
      plotOutput("loggrowthPlot")
    )
  ),
  
  helpText('This growth is also defined using the continuous (aka. differential) equation: $$\\frac{dN}{dt}=r_{max}N(\\frac{1-N}{K})$$'),
  helpText('\\(\\frac{dN}{dt}\\) is the population growth rate'),
  helpText('Once we treat this as a differential equation, we no longer need to worry about the midpoint parameter.'),
  helpText('Additionally, the continuous formulation allows us to simply consider interesting aspects of growth rates...'),
  helpText('What are the bottom two plots showing us?'),

  sidebarLayout(
    sidebarPanel(
      sliderInput("rmax",
                  "Maximum Growth Rate:",
                  min = 0,
                  max = 0.5,
                  value = 0.3,
                  step=0.025),
      sliderInput("Kdiff",
                  "Carrying Capacity:",
                  min = 0,
                  max = 1000,
                  value = 500),
    ),
    
  
    mainPanel(
     plotOutput("diffrPlot")
    )
   ),
  
  mainPanel(
    plotOutput("rmax")
  ) 
)

# Define server logic 
server <- function(input, output) {
  
  output$loggrowthPlot <- renderPlot({

    time<-seq(from=1, to=100, by=1)
    growth<-matrix(c(rep(NA,length(time))), nrow=length(time), ncol=1)
    
    for(i in 1:100){
      growth[i,1]<-input$K/(1+exp(-input$r*(time[i]-input$x0)))
    }
    
    
    plot_dat<-data.frame(time=time, growth=growth)

    ggplot(plot_dat, aes(x=time, y=growth))+
      geom_line(linewidth=1.5)+
      xlab("Time")+ylab("Population Size")+
      theme_bw()+coord_cartesian(ylim=c(0,1000))+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))
    
  })
  
  output$diffrPlot <- renderPlot({
    r<-input$rmax
    K<-input$Kdiff
    yini <- c(y = 1)
    derivs <- function(t, y, parms)
      list(r * y * (1-y/K))
    times <- seq(from = 1, to = 100, by = 0.1)
    out <- ode(y = yini, times = times, func = derivs,
               parms = NULL)
    
    plot_dat<-data.frame(time=out[,1], popsize=out[,2])
    
    ggplot(plot_dat, aes(x=time, y=popsize))+
      geom_line(linewidth=1.5)+
      xlab("Time")+ylab("Population Size")+
      theme_bw()+coord_cartesian(ylim=c(0,1000))+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))
    
    
  })
  
  output$rmax <- renderPlot({
    r<-input$rmax
    K<-input$Kdiff
    yini <- c(y = 1)
    derivs <- function(t, y, parms)
      list(r * y * (1-y/K))
    times <- seq(from = 1, to = 100, by = 0.1)
    out <- ode(y = yini, times = times, func = derivs,
               parms = NULL)
    
    growth<-matrix(NA, nrow=length(times), ncol=1)
    for(i in 2:length(times)){
      growth[i,1] = (out[i,2]-out[i-1,2])/0.1  
    }
    
    real_r<-growth[2:length(times),1]*(1/out[2:length(times),2])
    
    plot_dat<-data.frame(growth=real_r, popsize=out[2:length(times),2])
    
    p1<-ggplot(plot_dat, aes(x=popsize, y=real_r))+
      geom_line(linewidth=1.5)+
      geom_hline(yintercept=input$rmax, linetype="dashed", col="red")+
      geom_hline(yintercept=0, linetype="dashed")+
      #geom_text(x=-5, y=input$r-(input$r/10), label="Rmax", colour="red")+
      ylab("Per Capita Population Growth Rate")+xlab("Population Size")+
      theme_bw()+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))
    
    plot_dat<-data.frame(growth=growth[2:length(times),1], popsize=out[2:length(times),2])
    
    p2<-ggplot(plot_dat, aes(x=popsize, y=growth))+
      geom_line(linewidth=1.5)+
      #geom_hline(yintercept=input$r, linetype="dashed", col="red")+
      geom_vline(xintercept=input$Kdiff/2, linetype="dashed", col="red")+
      #geom_text(x=-5, y=input$r-(input$r/10), label="Rmax", colour="red")+
      ylab("Population Growth Rate")+xlab("Population Size")+
      theme_bw()+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))
    
    p1+p2
    
  }, width=800)
  
}



# Run the application 
shinyApp(ui = ui, server = server)
