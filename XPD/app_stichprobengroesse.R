library(shiny)

# Define UI for application
ui <- fluidPage(

   titlePanel("Stichprobengrösse"),
   
   sidebarLayout(
      sidebarPanel(
         # Slider für Stichprobengrösse
         sliderInput("n",
                     "Stichprobengrösse:",
                     min = 10,
                     max = 5000,
                     value = 50),
         # Anzahl Stichproben pro Versuch
         numericInput(
           "z",
           "Anzahl Stichproben",
           value = 100,
           min = 10,
           max = 1000,
           step = 1),
         # Auswahl für Messstation
         selectInput(inputId = "station", label = "Messstation",
                     choices = c("Langstrasse (Unterführung Nord) Richtung Helvetiaplatz", "Mythenquai Richtung Innenstadt","Hofwiesenstrasse Richtung Bucheggplatz"),
                     selected = "Langstrasse (Unterführung Nord) Richtung Helvetiaplatz") 
      ),
      
      # Anzeige
      mainPanel(
         h3(textOutput("title")),
         h4(textOutput("popMean")),
         h4(textOutput("sampleMean")),
         h4(textOutput("sampleSD")),
         plotOutput("histPlot")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Einlesen der Daten 
  # velo <- readRDS("../Woche14/data/Velo_Langstrasse_Nord.RDS")
  velo <- readRDS("data/velozaehlung.rds")
  
  # Funktion zur Erstellung einer Stichprobe der Grösse N
  # Rückgabe: Mittelwert der Stichprobe
  stichprobeN <- function(i, N, ort){
    stichprobe <- sample(velo[,ort],size=N,replace=T)
    sm <- mean(stichprobe)
    return(sm)
  }
  
  
  ## Berechnen der Mittelwerte von 100 Stichproben der Grösse N
  smp <- reactive({ sapply(1:input$z, stichprobeN, N = input$n, ort = input$station) })

  output$title <- renderText({ paste("Histogramm für",input$station) })
  output$popMean<- renderText({ paste("Populationsmittelwert", round(mean(velo[, input$station], na.rm = T),3)) })
  
  output$sampleMean <- renderText({ paste("Mittelwert der Stichprobenmittelwerte:", round(mean(smp(), na.rm = T),3)) })
  output$sampleSD <- renderText({ paste("Standardabweichung von", input$z, "Stichprobenmittelwerten:", round(sd(smp(), na.rm = T),3)) })
  output$histPlot <- renderPlot({
  
    ## Histogramm der Stichprobenmittelwerte
    print(dim(smp()))
    hist(smp(), main= paste("Stichprobengrösse", input$n),
         breaks = seq(0,100,0.5),
          xlab="Mittelwert", xlim=c(08,70), las = 1,
         col = "lightblue")
    abline(v = mean(velo[,input$station]), lwd = 2, col = "blue")
    abline(v = mean(smp(), na.rm = T))
     
   })
}

# Run Shiny-App
shinyApp(ui = ui, server = server)

