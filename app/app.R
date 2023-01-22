#Made by Filip Piskorski

library(shiny)
library(ggplot2)

ui <- fluidPage( #Setting up the gui
  titlePanel("Wioska dżdżownic"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "simulationCount",
                  label = "Ilość symulacji:",
                  min = 1000,
                  max = 100000,
                  value = 10000),
      checkboxInput("normal",
                    label = "Rozklad normalny/beta",
                    value = TRUE
      ),
      conditionalPanel("input.normal",
        sliderInput(inputId = "mean",
                    label = "Średnia:",
                    min = 100,
                    max = 300,
                    value = 200),
        sliderInput(inputId = "sd",
                    label = "Odchylenie standardowe:",
                    min = 10,
                    max = 100,
                    value = 40)
      ),
      conditionalPanel("!input.normal",
        sliderInput(inputId = "alpha",
                    label = "Alfa:",
                    min = 0,
                    max = 20,
                    value = 8,
                    step = 0.01),
        sliderInput(inputId = "beta",
                    label = "Beta:",
                    min = 0,
                    max = 20,
                    value = 1,
                    step = 0.01),
        sliderInput(inputId = "coeff",
                    label = "Współczynnik:",
                    min = 100,
                    max = 500,
                    value = 225,
                    step = 0.01),
        textOutput("meanInBeta")
      ),
      radioButtons(
        "CIType",
        "Wybierz typ użytego przedziału ufności: ",
        c(
          "Znane odchylenie standardowe" = "1",
          "Nieznane os, mała próba" = "00",
          "Nieznane os, duża próba" = "01"
        )
      ),
    sliderInput(inputId = "wormCount",
                label = "Ilość dżdżownic:",
                min = 2,
                max = 100,
                value = 50),
    sliderInput(inputId = "trustLevel",
                label = "Zakładany poziom ufności:",
                min = 0,
                max = 1,
                value = 0.95,
                step = 0.001),
    sliderInput(inputId = "howManyOnGraph",
                label = "Ilość symulacji na wykresie:",
                min = 1,
                max = 1000,
                value = 20,
                step = 1)
    ),
    mainPanel(
      textOutput("realTrustLevel"),
      plotOutput(outputId = "wormLengthHist"),
      plotOutput(outputId = "meansWithCV")
    )
  )
)

server <- function(input, output) {
  #Reading simulation parameters
  isNormal <- reactive({input$normal})
  CIType <- reactive({input$CIType})
  meanArg <- reactive(input$mean)
  sdArg <- reactive(input$sd)
  alpha <- reactive(input$alpha)
  beta <- reactive(input$beta)
  coeff <- reactive(input$coeff)
  simulationCount <- reactive(input$simulationCount)
  wormCount <- reactive(input$wormCount)
  trustLevel <- reactive(input$trustLevel)
  howManyOnGraph <- reactive(input$howManyOnGraph)
  
  #Calculating simulation measurements and means
  measurements <- reactive({
    if(isNormal()){
      matrix(rnorm(simulationCount()*wormCount(), meanArg(), sdArg()), simulationCount(), wormCount())
    } else {
      matrix(rbeta(simulationCount()*wormCount(), alpha(), beta())*coeff(), simulationCount(), wormCount())
    }
  })
  means <- reactive({
    result = 1:simulationCount()
    for(i in 1:simulationCount()){
      result[i] = mean(measurements()[i,])
    }
    result
  })
  
  #Calculating the mean and the standard deviation
  meanOfTheUsedDist <- reactive({
    if(isNormal()){
      meanArg()
    } else {
      alpha()/(alpha() + beta())*coeff()
    }
  })
  sdOfTheUsedDist <- reactive({
    if(isNormal()){
      sdArg()
    } else {
      sqrt(alpha()*beta()/((alpha() + beta())**2 * (alpha() + beta() + 1)))
    }
  })
  
  #Calculating error bars
  przedzialyUfnosci <- reactive({
    result = matrix(runif(simulationCount()*2,0,1), simulationCount(), 2)
    for (i in 1:simulationCount()) {
      if(CIType() == "01"){
        result[i, 1] = means()[i]-qnorm(1-(1-trustLevel())/2)*(sd(measurements()[i,]))/sqrt(wormCount()) 
        result[i, 2] = means()[i]+qnorm(1-(1-trustLevel())/2)*(sd(measurements()[i,]))/sqrt(wormCount())
      } else if(CIType() == "00"){
        result[i, 1] = means()[i]-qt(1-(1-trustLevel())/2,wormCount()-1)*(sd(measurements()[i,]))/sqrt(wormCount()) 
        result[i, 2] = means()[i]+qt(1-(1-trustLevel())/2,wormCount()-1)*(sd(measurements()[i,]))/sqrt(wormCount())
      } else {
        result[i, 1] = means()[i]-qnorm(1-(1-trustLevel())/2)*(sdOfTheUsedDist())/sqrt(wormCount()) 
        result[i, 2] = means()[i]+qnorm(1-(1-trustLevel())/2)*(sdOfTheUsedDist())/sqrt(wormCount())
      }
    }
    result
  })
  
  #Calculating success counts
  successCount <- reactive({
    sum = 0
    przUf <- przedzialyUfnosci()
    for (i in 1:simulationCount()) {
      if((przUf[i,1] <= meanOfTheUsedDist()) && (meanOfTheUsedDist() <= przUf[i,2]) ){
        sum = sum + 1
      }
    }
    sum
  })
  
  #Outputs
  output$realTrustLevel <- renderText({
    paste("Rzeczywisty poziom ufności = ", as.character(successCount()/simulationCount()))
  })
  ggplotData <- reactive({
    test <- 
    data.frame(x = 1:howManyOnGraph(),
              y = means()[1:howManyOnGraph()],
              lower = przedzialyUfnosci()[1:howManyOnGraph(),1],
              upper = przedzialyUfnosci()[1:howManyOnGraph(),2])
  })
  output$meansWithCV <- renderPlot({
    ggplot() +
    geom_point(data = ggplotData(), mapping = aes(x, y)) +
    geom_errorbar(data = ggplotData(), mapping = aes(x, y, ymin = lower, ymax = upper)) +
      geom_hline(
        yintercept = meanOfTheUsedDist()
      )
  })
  output$meanInBeta <- renderText({paste("Wartosc oczekiwana = ", as.character(alpha()/(alpha() + beta())*coeff()))})
  output$wormLengthHist <- renderPlot({
    hist(measurements())
  })
}

shinyApp(ui = ui, server = server)