# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # Titel der App
  titlePanel("Obesity"),
  
  # Layout für die Eingaben in die App und die Ausgaben
  sidebarLayout(
    
    # Die Definition der Eingabefelder auf der linken Seite
    sidebarPanel(
      

      radioButtons("gender", h5("What is your gender?"),
                   choices = list("Female" = "Female", "Male" = "Male")),
      
      numericInput(inputId="age", 
                   label="What is your age?:", 
                   value = 20,
                   min=0,max=120,step=1
      ),
      numericInput(inputId="height", 
                   label="What is your height? (in meters)", 
                   value = 1.80,
                   min=0,max=3.00,step=0.01
      ),
      numericInput(inputId="weight", 
                   label="What is your weight? (in kilograms)", 
                   value = 90,
                   min=0,max=500,step=1
      ),
      
      radioButtons("favc", h5("Do you eat high caloric food frequently?"),
                   choices = list("Yes" = "yes", "No" = "no")),
      
      radioButtons("fcvc", h5("Do you usually eat vegetables in your meals?"),
                    choices = list("Never" = 1, "Sometimes" = 2, "Always"=3)),
      
      radioButtons("ncp", h5("How many main meals do you have daily?"),
                   choices = list("Between 1 and 2" = 1, "3" = 2, "More than 3" = 3)),
      
      selectInput("caec", h5("Do you eat any food between meals?"),
                   choices = list("No" = "no", "Sometimes" = "Sometimes", "Frequently" = "Frequently", "Always" = "Always")),
      
      radioButtons("ch2o", h5("How much water do you drink daily?"),
                   choices = list("Less than a liter" = 1, "Between 1 and 2 L" = 2, "More than 2 L" = 3)),
      
      selectInput("faf", h5("How often do you have physical activity?"),
                  choices = list("I do not have" = 0, "1 or 2 days" = 1, "2 or 4 days" = 2, "4 or 5 days" = 3)),
      
      radioButtons("tue", h5("How much time do you use technological devices such as cell phone, videogames, television, computer and others per day?"),
                   choices = list("0-2 hours" = 0, "3-5 hours" = 1, "More than 5 hours" = 2)),
      
      selectInput("calc", h5("How often do you drink alcohol?"),
                  choices = list("I do not drink" = "no", "Sometimes" = "Sometimes", "Frequently" = "Frequently", "Always" = "Always"),selected = 1),
      
    ),
    
    # der Hauptbereich der Nutzeroberfläche für die Ausgabe der Ergebnisse
    mainPanel(
      
      # Ausgabe des Histogramms
      plotOutput(outputId = "Model"),
      
      # Ausgabe der Prognose
      htmlOutput("Prognose"),
      
    )
  )
)

server <- function(input, output) {
  
  # Innerhalb dieser Funktion werden die Bilder für die Ausgabe
  # erzeugt und die Ergebnisse berechnet
  
  # Folgende Funktion berechnet die Prognose für die eingegeben Werte  
  prognose <- reactive({
    
    # Speichere die Daten der Einflussvariablen in ein Objekt X
    X <- obesity_data[,c("Gender", "Age","FAVC","FCVC","NCP","CAEC","CH2O","FAF","TUE","CALC")]
    
    # Ersetze die erste Zeile in X nun mit den neuen, eingegebenen Werten
    
    X[1, "Age"] <- input$age
    
    
    X[1, "Gender"] <- as.factor(input$gender)
    X[1, "FAVC"] <- as.factor(input$favc)
    X[1, "FCVC"] <- as.factor(input$fcvc)
    X[1, "NCP"] <- as.factor(input$ncp)
    X[1, "CAEC"] <- as.factor(input$caec)
    X[1, "CH2O"] <- as.factor(input$ch2o)
    X[1, "FAF"] <- as.factor(input$faf)
    X[1, "TUE"] <- as.factor(input$tue)
    X[1, "CALC"] <- as.factor(input$calc)
    
    # Berechne die Prognosen für X
    # die Prognose der neuen, eingegebenen Werte stehen im ersten Eintrag des Prognosevektors
    prognosevektor <- predict(model, X)
    prog <- c(prognosevektor[1, 1:7])
    prog <- round(prog, digits = 2)
    
    
    predicted_bmi <- which.max(prog) - 1
    
    # print(prog)
    
    predicted_bmi
  })         
  
  
  output$Prognose <- renderUI({
    prog <- prognose()
    
    actual_bmi <- round(input$weight / input$height^2, digits = 2)
    
    weight_levels <- c("Underweight", "Normal", "Overweight I", "Overweight II", "Obese I", "Obese II", "Obese III")
    weight_values <- c(0, 18.5, 24.9, 27.9, 29.9, 34.9, 39.9, 40)
    
    actual_level = "NULL"
    
    for (i in 1:length(weight_values)) {
      if (actual_bmi < weight_values[i]) {
        actual_level = weight_levels[i-1]
        break
      }
    }

    output <- HTML(paste("Predicted Level: ", weight_levels[prog], "<br>", "Actual Level: ", actual_level, "<br>", "Actual BMI:", actual_bmi))
    
    
    
  })
  
  output$Model <- renderPlot({
    plot(model)
    text(model)
  })
  
}

shinyApp(ui = ui, server = server)