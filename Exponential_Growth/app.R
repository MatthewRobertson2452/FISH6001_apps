

library(shiny)
library(ggplot2)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Exponential Growth"),
    
    #Text and equations 
    withMathJax(),
    helpText('Exponential population growth is defined by the equation $$N_t=N_0e^{r_{max}t}$$'),
    helpText('\\(N_0\\) is the initial population size'),
    helpText('\\(r_{max}\\) is the maximum rate of growth'),
    helpText('\\(t\\) is the number of time steps'),
    
    helpText('This growth is also defined using the differential equation: $$\\frac{dN}{dt}=rN$$'),
    helpText('\\(\\frac{dN}{dt}\\) is the population growth rate'),
    helpText('\\(r_{max}\\) is still the rate of growth'),
    helpText('\\(N\\) is the population size'),
    helpText('Regardless, both equations, produce the same output:'),
    # Sidebar with a slider input 
    sidebarLayout(
        sidebarPanel(
            sliderInput("x0",
                        "Initial Size:",
                        min = 0,
                        max = 100,
                        value = 1),
            sliderInput("r",
                        "Growth Rate:",
                        min = 0,
                        max = 0.5,
                        value = 0.3,
                        step=0.025),
            sliderInput("x",
                        "Time:",
                        min = 0,
                        max = 100,
                        value = 30)
        ),

        mainPanel(
           plotOutput("expgrowthPlot")
        )
    )
    # mainPanel(
    #   plotOutput("diffexpgrowthPlot")
    # )
)

# Define server logic 
server <- function(input, output) {

    output$expgrowthPlot <- renderPlot({

        time<-seq(from=1, to=input$x, by=1)
        growth<-matrix(c(rep(NA,length(time))), nrow=length(time), ncol=1)
        
        for(i in 1:input$x){
          growth[i,1]<-input$x0*(1+input$r)^time[i]
        }
        
    plot_dat<-data.frame(time=time, growth=growth)
    
        ggplot(plot_dat, aes(x=time, y=growth))+
          geom_line(linewidth=1.5)+
          xlab("Time")+ylab("Population Size")+
          theme_bw()+coord_cartesian(ylim=c(0,input$x0+1000))+
          theme(axis.text=element_text(size=14),
                axis.title=element_text(size=14,face="bold"))
          
    })
    
    output$diffexpgrowthPlot <- renderPlot({

      difftime<-seq(from=1, to=input$x, by=1)
      diffgrowth<-matrix(c(input$x0,rep(NA,length(difftime)-1)), nrow=length(difftime), ncol=1)
      
      for(i in 2:input$x){
        diffgrowth[i,1]<-input$r*diffgrowth[i-1,1]+diffgrowth[i-1,1]
      }
      
      diffplot_dat<-data.frame(time=difftime, growth=diffgrowth)

      ggplot(diffplot_dat, aes(x=time, y=growth))+
        geom_line(linewidth=1.5)+
        xlab("Time")+ylab("Population Size")+
        theme_bw()+coord_cartesian(ylim=c(0,input$x0+1000))+
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=14,face="bold"))
      
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
