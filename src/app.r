# define light and dark modes

light_mode <- bs_theme(bootswatch = "flatly", version = 5)
dark_mode <- bs_theme(bootswatch = "darkly", version = 5)

# define app UI
ui <- fluidPage(
  
  # set app title
  titlePanel("Obesity"),
  
  # set default theme
  theme = light_mode,
  
  # use sidebar layout template
  sidebarLayout(
    
    # define input fields on the left
    sidebarPanel(
  
      tabsetPanel(type = "tabs",
                  
        tabPanel("General",
                  
          radioButtons("gender", h5("What is your gender?"),
                       choices = list("Female" = "Female", "Male" = "Male")),
          
          numericInput(inputId="age", 
                       label="What is your age?:", 
                       value = 20,
                       min=0,max=120,step=1),
          
          numericInput(inputId="height", 
                       label="What is your height? (in meters)", 
                       value = 1.80,
                       min=0,max=3.00,step=0.01),
          
          numericInput(inputId="weight", 
                       label="What is your weight? (in kilograms)", 
                       value = 90,
                       min=0,max=500,step=1),
          
          selectInput("faf", h5("How often do you have physical activity?"),
                      choices = list("I do not have" = 0, "1 or 2 days" = 1, "2 or 4 days" = 2, "4 or 5 days" = 3)),
          
          radioButtons("tue", h5("How much time do you use technological devices such as cell phone, videogames, television, computer and others per day?"),
                       choices = list("0-2 hours" = 0, "3-5 hours" = 1, "More than 5 hours" = 2))
        ),
                  
        tabPanel("Habits",
                 
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
         
         selectInput("calc", h5("How often do you drink alcohol?"),
                     choices = list("I do not drink" = "no", "Sometimes" = "Sometimes", "Frequently" = "Frequently", "Always" = "Always"),selected = 1)
         ),
        
        tabPanel("Settings",
        
          h5(materialSwitch("dark_mode", "Dark mode"))),
        
        tabPanel("Help", p("placeholder text"))
        )
      ),
      
      
    # der Hauptbereich der Nutzeroberfläche für die Ausgabe der Ergebnisse
    mainPanel(
      
      # output model
      plotOutput(outputId = "Model"),
      
      br(),
      
      htmlOutput("Prediction"),
      
      br(),
      
      tabsetPanel(type = "tabs", 
                  tabPanel("BMI Table", tableOutput("bmiTable")),
                  tabPanel("Variables", tableOutput("varTable"))),
    )
  )
)

# define app logic
server <- function(input, output, session) {
  
  # define static variables
  X <- obesity_data[,c("Gender", "Age","FAVC","FCVC","NCP","CAEC","CH2O","FAF","TUE","CALC")]
  bmi_levels <- c("Underweight", "Normal", "Overweight I", "Overweight II", "Obese I", "Obese II", "Obese III")
  bmi_values_lower <- c(0, 18.5, 25, 27.5, 30, 35, 40)
  bmi_values_upper <- c(18.5, 25, 27.5, 30, 35, 40, 50)
  bmi_table <- data.frame(bmi_code = c(seq(0,6)), bmi_levels = bmi_levels, bmi_values_lower = bmi_values_lower, bmi_values_upper = bmi_values_upper)
  
  actual_level = "NULL"
  
  var_names <- c("FCVC", "CAEC", "CALC")
  var_meaning <- c("Frequency of vegetable consumption", "Frequency of eating between meals", "Frequency of drinking alcohol")
  var_table <- data.frame(names = var_names, meaning = var_meaning)
  
  # predict obesity value for input values  
  predict_bmi <- reactive({
    
    # replace values in first row with input values
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
    
    # calculate prediction for X
    pred <- predict(pruned, X, type = "class")
    
    # predicted bmi from input is in the first row
    predicted_bmi <- pred[1]
    
    predicted_bmi
  })         
  
  # show predicted and actual values
  output$Prediction <- renderUI({
    
    predicted_bmi <- predict_bmi()
    
    actual_bmi <- round(input$weight / input$height^2, digits = 2)
    
    for (i in 1:length(bmi_values_lower)) {
      if (actual_bmi < bmi_values_lower[i]) {
        actual_level = bmi_levels[i-1]
        break
      }
    }

    output <- HTML(paste("Predicted Level: ", bmi_levels[predicted_bmi], "<br>", "Actual Level: ", actual_level, "<br>", "Actual BMI:", actual_bmi))
    
  })
  
  # show plotted model
  output$Model <- renderPlot({
    
    # par(bg = NA)
    rpart.plot(pruned, type = 3, uniform = TRUE, box.palette = "Blues", digits = 1, fallen.leaves = TRUE, varlen = 0, faclen = 0, extra = 0, tweak = 1.15)
    # plot(model)
    # text(model)
    
  })
  
  # show static tables
  output$bmiTable <- renderTable(bmi_table, striped = TRUE, digits = 1)
  output$varTable <- renderTable(var_table, striped = TRUE)
  
  # toggle dark mode
  observe(session$setCurrentTheme(
    if(isTRUE(input$dark_mode)) dark_mode else light_mode
  ))
  
}

shinyApp(ui = ui, server = server)