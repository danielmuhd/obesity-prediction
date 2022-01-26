# define light and dark modes

light_mode <- bs_theme(bootswatch = "flatly", version = 5)
dark_mode <- bs_theme(bootswatch = "darkly", version = 5)

# define app UI
ui <- fluidPage(
  
  # set app title
  titlePanel("Obesity Risk Prediction"),
  
  # set default theme
  theme = light_mode,
  
  # use sidebar layout template
  sidebarLayout(
    
    # define input fields on the left
    sidebarPanel(
  
      tabsetPanel(type = "tabs",
                  
        tabPanel("General",
                  
          # radioButtons("gender", h5("What is your gender?"),
          #              choices = list("Female" = "Female", "Male" = "Male")),
          # 
          # numericInput(inputId="age", 
          #              label=h5("What is your age?"), 
          #              value = 20,
          #              min=0,max=120,step=1),
          
          numericInput(inputId = "height", 
                       label = h5("What is your height? (in meters)"), 
                       value = 1.80,
                       min = 0.50, max = 3.00, step = 0.01),
          
          numericInput(inputId = "weight", 
                       label = h5("What is your weight? (in kilograms)"), 
                       value = 90,
                       min = 20, max = 500, step = 1),
          
          radioButtons(inputId = "familyHistoryWithOverweight", 
                       label = h5("Do you have family members who are overweight?"),
                       choices = list("Yes" = "yes", "No" = "no")),
          
          selectInput(inputId = "exercise", 
                      label = h5("How often do you carry out physical activities every week?"),
                      choices = list("0 days" = 0, "1 to 2 days" = 1, "2 to 4 days" = 2, "4 to 5 days" = 3)),
          
          radioButtons(inputId = "screens", 
                       label = h5("How much time do you spend looking at screens (smartphones, computers, videogames, etc.) per day?"),
                       choices = list("0 to 2 hours" = 0, "3 to 5 hours" = 1, "More than 5 hours" = 2))
          
        ),
                  
        tabPanel("Habits",
                 
          radioButtons(inputId = "highCalories", 
                      label = h5("Do you eat high caloric food frequently?"),
                      choices = list("Yes" = "yes", "No" = "no")),
         
          radioButtons(inputId = "veggies", 
                      label = h5("Do you usually eat vegetables in your meals?"),
                      choices = list("Never" = 1, "Sometimes" = 2, "Always"=3)),
         
          radioButtons(inputId = "water", 
                      label = h5("How much water do you drink daily?"),
                      choices = list("Less than a litres" = 1, "Between 1 and 2 litres" = 2, "More than 2 litres" = 3)),
         
          selectInput(inputId = "alcohol", 
                      label = h5("How often do you drink alcohol?"),
                      choices = list("I do not drink" = "no", "Sometimes" = "Sometimes", "Frequently" = "Frequently", "Always" = "Always"))
       ),
        
        tabPanel("Settings",
        
          h5(materialSwitch("dark_mode", "Dark mode"))
       ),
        
        tabPanel("Help", 
                 
          HTML("<h4>Description</h4> 
                <p>Predicts your risk of obesity.</p>
                <h4>Usage</h4>
                <ol>
                <li>Answer the questions in the <code>General</code> and <code>Habits</code> tab.</li>
                <li>Check <code>Predicted Risk Level</code> for your result.</li>
                <li>For more context, follow the decision tree to see how your result is calculated.</li>
                </ol>
                <h4>Tree Interpretation</h4>
                <p>Follow the paths and reach the end node.<br>
                Check the <code>Variables</code> table for more details on the tree labels.</p>
                <p><code>0</code> = not at risk of obesity<br>
                <code>1</code> = at risk of obesity</p>"))
      )
    ),
      
    # define output sections on the right
    mainPanel(
      
      # output plotted model
      plotOutput(outputId = "model"),
      
      br(),
      
      htmlOutput(outputId = "prediction"),
      
      br(),
      
      tabsetPanel(type = "tabs", 
                  tabPanel("BMI Table", 
                           tableOutput("bmiTable")),
                  
                  tabPanel("Variables", 
                           tableOutput("varTable")),
      )
    )
  )
)

# define app logic
server <- function(input, output, session) {
  
  # define static variables
  X <- df.test[,c("familyHistoryWithOverweight", "highCalories", "veggies",
                  "water", "exercise", "screens", "alcohol")]
  
  bmi_levels <- c("Underweight", "Normal", "Overweight I", "Overweight II", 
                  "Obese I", "Obese II", "Obese III")
  
  bmi_values_range <- c("Below 18.5", "18.5 - 24.9", "25.0 - 27.5", "27.6 - 29.9",
                        "30.0 - 34.9", "35.0 - 39.9", "Over 40")
  
  bmi_values_lower <- c(0, 18.5, 25, 27.5, 30, 35, 40)
  
  bmi_table <- data.frame(Level = bmi_levels, Range = bmi_values_range)
  
  risk_levels = c("Not at risk", "At risk")
  
  
  actual_level = "NULL"
  
  var_names <- c("veggies", "water", "exercise", "screens")
  
  var_meaning <- c("Frequency of vegetable consumption ", 
                   "Sum of daily water intake", 
                   "Frequency of weekly physical activity", 
                   "Time spent looking at screens per day")
  
  var_values <- c("1 = Never<br>2 = Sometimes<br>3 = Always",
                  "1 = Less than 1 litre<br>2 = Between 1 and 2 litres<br>3 = More than 2 litres",
                  "0 = No exercise<br>1 = 1 to 2 days<br>2 = 2 to 4 days<br>3 = 4 to 5 days",
                  "0 = 0 to 2 hours<br>1 = 3 to 5 hours<br>2 = More than 5 hours")
  
  var_table <- data.frame(Names = var_names, Meaning = var_meaning, Values = var_values)
  
  # predict risk of obesity for input values  
  predict_risk <- reactive({
    
    # replace values in first row with input values
    # X[1, "Age"] <- input$age
    # X[1, "Gender"] <- as.factor(input$gender)
    X[1, "familyHistoryWithOverweight"] <- as.factor(input$familyHistoryWithOverweight)
    X[1, "highCalories"] <- as.factor(input$highCalories)
    X[1, "veggies"] <- as.factor(input$veggies)
    X[1, "water"] <- as.factor(input$water)
    X[1, "exercise"] <- as.factor(input$exercise)
    X[1, "screens"] <- as.factor(input$screens)
    X[1, "alcohol"] <- as.factor(input$alcohol)
    
    # calculate prediction for X
    pred <- predict(fit, X, type = "prob")
    
    # predicted risk from input is in the first row
    predicted_risk <- as.numeric(colnames(pred)[apply(pred, 1, which.max)][1])
    predicted_prob <- round(max(pred[1,]), 2)

    predictions <- c(predicted_risk, predicted_prob)
  })         
  
  # show predicted and actual values
  output$prediction <- renderUI({
    
    predictions <- predict_risk()
    
    actual_bmi <- round(input$weight / input$height^2, digits = 2)
    
    for (i in 1:length(bmi_values_lower)) {
      if (actual_bmi < bmi_values_lower[i]) {
        actual_level = bmi_levels[i - 1]
        break
      }
    }

    output <- HTML(paste("Predicted Risk Level: ", risk_levels[predictions[1] + 1], "<br>", 
                         "Predicted Probability: ", predictions[2], "<br>", 
                         "Actual BMI Level: ", actual_level, "<br>", 
                         "Actual BMI Value:", actual_bmi))
  })
  
  # show plotted model
  output$model <- renderPlot({
    
    rpart.plot(fit, type = 3, uniform = TRUE, box.palette = "Blues", digits = 1, fallen.leaves = TRUE, varlen = 0, faclen = 0, extra = 0, tweak = 1.15)
  })
  
  # show static tables
  output$bmiTable <- renderTable(bmi_table, striped = TRUE, digits = 1)
  
  output$varTable <- renderTable(var_table, striped = TRUE, sanitize.text.function = identity)
  
  # toggle dark mode
  observe(session$setCurrentTheme(
    
    if(isTRUE(input$dark_mode)) dark_mode else light_mode
  ))
  
}

shinyApp(ui = ui, server = server)
